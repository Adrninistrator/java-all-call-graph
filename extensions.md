# 1. java-callgraph2提供的扩展功能

java-callgraph2提供的扩展功能在获取方法调用关系阶段执行，因此新增或修改了扩展功能的代码后，需要重新获取方法调用关系，即调用`TestRunnerWriteDb`类

## 1.1. 人工添加缺失的方法调用关系（定制化代码开发）

在某些情况下，会存在方法调用关系缺失的情况，可通过定制的代码开发，人工添加缺失的方法调用关系

方法调用关系缺失的情况，通常会涉及到实现或继承

### 1.1.1. 使用方式

- 自定义处理类开发

开发自定义处理类，继承自以下类

```
com.adrninistrator.jacg.extensions.code_parser.AbstractManualAddCallGraphSimpleParser
```

实现以下方法，根据实际情况返回相应的值

|方法|返回值|说明|
|---|---|---|
|chooseTopSuperOrItfClassFullName|String|指定父类或接口完整类名|
|chooseExtendsOrImpl|boolean|指定是继承类还是实现接口的方式，返回true代表继承类；false代表实现接口|
|chooseCalleeMethodName|String|指定识别到什么名称的方法被调用时，需要补充缺失的调用关系，不需要指定括号或参数|
|chooseAddCalleeMethodName|String|指定需要补充的调用关系的被调用方法名，不需要指定括号或参数|
|chooseAddCalleeMethodNameArgs|String|指定需要补充的调用关系的被调用方法参数，需要包含括号，各参数也需要为完整类名形式，使用半角逗号,分隔，中间不能出现空格|

以上方法的示例见下文

- 在配置文件中指定以上类

在“resources/~jacg_extensions/code_parser.properties”配置文件中指定以上自定义处理类的完整类名，可指定多个类，如下所示：

```
test.extensions.code_parser.MACGActionListenerParser
test.extensions.code_parser.MACGFixedService1Parser
test.extensions.code_parser.MACGUnfixedService1Parser
```

- 重新获取方法调用关系

重新执行`TestRunnerWriteDb`类

### 1.1.2. 不同情况的示例

以下示例对应的测试入口类在test.manual_add_call_graph包中，如下所示：

|类名|作用|
|---|---|
|TestMACG0RunnerWriteDbWithOutExtensions|生成方法调用关系并写入数据库，不添加自定义处理类|
|TestMACG1RunnerWriteDbWithExtensions|生成方法调用关系并写入数据库，添加以下自定义处理类|
|TestMACGRunnerGenAllGraph4Callee|生成方法向上的完整调用链|
|TestMACGRunnerGenAllGraph4Caller|生成方法向下的完整调用链|

在以下示例中，假如在生成方法调用关系并写入数据库时，没有添加以下自定义处理类，则生成的方法调用链可能出现以下问题：

a. 部分方法调用关系缺失，例如不存在调用ActionListener实现类的actionPerformed()方法的调用关系
b. 方法向上的完整调用链中，出现多余的方法调用关系，例如AbstractFixedService1a的子类的invoke()方法，均调用了AbstractFixedService1a的invoke()方法

#### 1.1.2.1. 实现接口的情况

在awt中，为Button等Component子类实例添加动作监听器时，需要调用addActionListener()方法，传入ActionListener接口实现类

ActionListener接口实现类需要实现actionPerformed(ActionEvent e)方法，在该方法中进行动作发生时的处理

假如存在以下ActionListener接口实现类ActionListener1

```java
public class ActionListener1 implements ActionListener {
    @Override
    public void actionPerformed(ActionEvent e) {
        System.getProperty("");
    }
}
```

在其他方法中创建了ActionListener1类的实例，并通过Button的addActionListener()方法添加到动作监听器中

```java
new Button().addActionListener(new ActionListener1());
```

若不进行专门的处理，在生成的方法调用关系中，其他方法调用ActionListener1实现类的actionPerformed()方法的调用关系会缺失

为ActionListener接口实现类补充缺失调用方法的类为MACGActionListenerParser，如下所示：

```java
public class MACGActionListenerParser extends AbstractManualAddCallGraphSimpleParser {
    @Override
    public String chooseTopSuperOrItfClassFullName() {
        // ActionListener接口的完整类名
        return "java.awt.event.ActionListener";
    }

    @Override
    protected boolean chooseExtendsOrImpl() {
        // 在使用ActionListener时，是实现接口的方式，返回false
        return false;
    }

    @Override
    protected String chooseCalleeMethodName() {
        // 当ActionListener接口实现类的构造函数被调用时，添加缺失的方法调用关系，该方法名作为调用方法
        return "<init>";
    }

    @Override
    protected String chooseAddCalleeMethodName() {
        // 添加缺失的方法调用关系，被调用方法为actionPerformed
        return "actionPerformed";
    }

    @Override
    protected String chooseAddCalleeMethodNameArgs() {
        // 添加缺失的方法调用关系，被调用方法参数类型为java.awt.event.ActionEvent
        return "(java.awt.event.ActionEvent)";
    }
}
```

以上类在生成的方法调用关系文件中增加的调用关系如下所示：

```
M:9 test.call_graph.action_listener.ActionListener1:<init>() (MA)test.call_graph.action_listener.ActionListener1:actionPerformed(java.awt.event.ActionEvent) 0 1
M:16 test.call_graph.action_listener.TestActionListener$1:<init>(test.call_graph.action_listener.TestActionListener) (MA)test.call_graph.action_listener.TestActionListener$1:actionPerformed(java.awt.event.ActionEvent) 0 1
M:20 test.call_graph.action_listener.TestActionListener$2:<init>(test.call_graph.action_listener.TestActionListener) (MA)test.call_graph.action_listener.TestActionListener$2:actionPerformed(java.awt.event.ActionEvent) 0 1
```

#### 1.1.2.2. 继承父类的情况

抽象类AbstractFixedService1中存在invoke()方法与抽象方法execute()，在invoke()方法中会调用execute()方法，在其子类中需要实现execute()方法，在调用AbstractFixedService1的子类时，需要调用其invoke()方法，如下所示：（该类实现Runnable接口仅用于验证这种情况下抽象类如有实现接口也能正常处理）

```java
public String invoke() {
    return execute();
}

protected abstract String execute();
```

其他方法使用AbstractFixedService1子类的示例如下：

```java
public String test2(String s) {
    return new AbstractFixedService1() {
        @Override
        protected String execute() {
            return String.valueOf(System.currentTimeMillis());
        }
    }.invoke();
}
```

若不进行专门的处理，在生成的方法调用关系中，其他方法调用AbstractFixedService1子类的execute()方法的调用关系会不准确

为AbstractFixedService1子类补充缺失调用方法的类为MACGFixedService1Parser，如下所示：

```java
public class MACGFixedService1Parser extends AbstractManualAddCallGraphSimpleParser {
    @Override
    public String chooseTopSuperOrItfClassFullName() {
        // AbstractFixedService1类的完整类名
        return "test.call_graph.manual_add_callgraph.fixed.AbstractFixedService1";
    }

    @Override
    protected boolean chooseExtendsOrImpl() {
        // 在使用AbstractFixedService1时，是继承父类的方式，返回true
        return true;
    }

    @Override
    protected String chooseCalleeMethodName() {
        // 当AbstractFixedService1子类的invoke()方法被调用时，添加缺失的方法调用关系，该方法名作为调用方法
        return "invoke";
    }

    @Override
    protected String chooseAddCalleeMethodName() {
        // 添加缺失的方法调用关系，被调用方法为execute
        return "execute";
    }

    @Override
    protected String chooseAddCalleeMethodNameArgs() {
        // 添加缺失的方法调用关系，被调用方法参数为空
        return "()";
    }
}
```

以上类在生成的方法调用关系文件中增加的调用关系如下所示：

```
M:162 test.call_graph.manual_add_callgraph.fixed.FixedService1b:invoke() (MA)test.call_graph.manual_add_callgraph.fixed.FixedService1b:execute() 0 1
M:165 test.call_graph.manual_add_callgraph.fixed.TestFixedManualAddCallGraph$1:invoke() (MA)test.call_graph.manual_add_callgraph.fixed.TestFixedManualAddCallGraph$1:execute() 0 1
M:168 test.call_graph.manual_add_callgraph.fixed.TestFixedManualAddCallGraph$2:invoke() (MA)test.call_graph.manual_add_callgraph.fixed.TestFixedManualAddCallGraph$2:execute() 0 1
M:171 test.call_graph.manual_add_callgraph.fixed.TestFixedManualAddCallGraph$3:invoke() (MA)test.call_graph.manual_add_callgraph.fixed.TestFixedManualAddCallGraph$3:execute() 0 1
M:174 test.call_graph.manual_add_callgraph.fixed.TestFixedManualAddCallGraph$4:invoke() (MA)test.call_graph.manual_add_callgraph.fixed.TestFixedManualAddCallGraph$4:execute() 0 1
```

#### 1.1.2.3. 继承父类的情况（方法参数使用范型）

抽象类AbstractUnFixedService1中存在invoke()方法与抽象方法execute()，在invoke()方法中会调用execute()方法，在其子类中需要实现execute()方法，在调用AbstractUnFixedService1的子类时，需要调用其invoke()方法，以上方法参数使用了范型，如下所示：

```java
public abstract class AbstractUnFixedService1<Req, Rsp extends Collection> {
    public Rsp invoke(Req req, Rsp rsp) {
        return execute(req, rsp);
    }

    protected abstract Rsp execute(Req req, Rsp rsp);
}
```

AbstractUnFixedService1类的子类（例如UnfixedService1a）在编译后，编译器会增加一个包含ACC_BRIDGE、ACC_SYNTHETIC标记，方法名也是execute的方法

编译器增加的execute方法中，参数类型为范型对应的类型，例如Req对应Object类型，Rsp extends Collection对应Collection类型，`该方法会调用子类实现的execute方法`

例如子类UnfixedService1a使用的参数类型分别为Long、LinkedList，则编译器增加的execute方法如下所示

```java
protected synthetic bridge Collection execute(Object req, Collection rsp) {
    Long l = (Long) req;
    LinkedList list = (LinkedList) rsp;
    return this.execute(l, list);
}
```

总结如下：`方法参数使用了范型的抽象父类的子类中，编译器会自动为使用了范型参数的方法增加同名方法，该方法的参数类型与范型类型一致，在该方法中会调用子类实现的实际方法。因此可以增加一个调用子类方法的固定类型参数的方法，即可关联到子类的参数不固定的实现方法`

其他方法使用AbstractUnFixedService1子类的示例如下：

```java
public ArrayList test2() {
    return new AbstractUnFixedService1<Integer, ArrayList>() {
        @Override
        protected ArrayList execute(Integer integer, ArrayList list) {
            System.setProperty("", "");
            return null;
        }
    }.invoke(null, null);
}
```

若不进行专门的处理，在生成的方法调用关系中，其他方法调用AbstractUnFixedService1子类的execute()方法的调用关系会不准确

为AbstractUnFixedService1子类补充缺失调用方法的类为MACGFixedService1Parser，如下所示：

```java
public class MACGFixedService1Parser extends AbstractManualAddCallGraphSimpleParser {
    @Override
    public String chooseTopSuperOrItfClassFullName() {
        // AbstractFixedService1类的完整类名
        return "test.call_graph.manual_add_callgraph.fixed.AbstractFixedService1";
    }

    @Override
    protected boolean chooseExtendsOrImpl() {
        // 在使用AbstractFixedService1时，是继承父类的方式，返回true
        return true;
    }

    @Override
    protected String chooseCalleeMethodName() {
        // 当AbstractFixedService1子类的invoke()方法被调用时，添加缺失的方法调用关系，该方法名作为调用方法
        return "invoke";
    }

    @Override
    protected String chooseAddCalleeMethodName() {
        // 添加缺失的方法调用关系，被调用方法为execute
        return "execute";
    }

    @Override
    protected String chooseAddCalleeMethodNameArgs() {
        // 添加缺失的方法调用关系，被调用方法参数为空
        return "()";
    }
}
```

以上类在生成的方法调用关系文件中增加的调用关系如下所示：

```
M:189 test.call_graph.manual_add_callgraph.unfixed.UnfixedService1a:invoke(java.lang.Object,java.util.Collection) (MA)test.call_graph.manual_add_callgraph.unfixed.UnfixedService1a:execute(java.lang.Object,java.util.Collection) 0 1
M:194 test.call_graph.manual_add_callgraph.unfixed.TestUnfixedManualAddCallGraph$1:invoke(java.lang.Object,java.util.Collection) (MA)test.call_graph.manual_add_callgraph.unfixed.TestUnfixedManualAddCallGraph$1:execute(java.lang.Object,java.util.Collection) 0 1
M:197 test.call_graph.manual_add_callgraph.unfixed.TestUnfixedManualAddCallGraph$2:invoke(java.lang.Object,java.util.Collection) (MA)test.call_graph.manual_add_callgraph.unfixed.TestUnfixedManualAddCallGraph$2:execute(java.lang.Object,java.util.Collection) 0 1
M:200 test.call_graph.manual_add_callgraph.unfixed.TestUnfixedManualAddCallGraph$3:invoke(java.lang.Object,java.util.Collection) (MA)test.call_graph.manual_add_callgraph.unfixed.TestUnfixedManualAddCallGraph$3:execute(java.lang.Object,java.util.Collection) 0 1
```

# 2. java-all-call-graph提供的扩展功能

java-all-call-graph提供的扩展功能在生成方法向上/向下完整调用链阶段执行，因此新增或修改了扩展功能的代码后，需要重新生成方法向上/向下完整调用链，即调用`TestRunnerGenAllGraph4Callee`等类

## 2.1. 处理方法上的注解信息的扩展功能

在method_annotation_handler.properties配置文件中，可以定义用于对方法上的注解进行处理的类完整类名，该文件每行指定一项配置，可指定多行

对方法上的注解进行处理的类需要继承自com.adrninistrator.jacg.extensions.annotation_handler.AbstractAnnotationHandler，并实现以下方法

|方法名|方法作用|
|---|---|
|checkHandleAnnotation|判断当前类是否处理对应的注解|
|handleAnnotation|返回方法上的注解处理后的结果|

本工具在生成方法完整调用链时，会先遍历method_annotation_handler.properties配置文件中指定所有的AbstractAnnotationHandler子类，即对方法上的注解进行处理的类，调用checkHandleAnnotation判断当前类是否会处理的注解，若是则调用handleAnnotation方法获取处理后的注解信息。

最后会调用默认的方法注解处理类com.adrninistrator.jacg.extensions.annotation_handler.DefaultAnnotationHandler进行处理，该类会处理所有的注解，生成的注解信息格式为“@注解类名”，例如“@org.aspectj.lang.annotation.Around”

`假如一个方法上存在多个注解，则每个注解的信息会按照注解类名升序排序后，依次拼接在方法信息后`

DefaultAnnotationHandler类不需要在method_annotation_handler.properties配置文件中指定

- 支持显示Spring MVC的@RequestMapping等注解中的路径信息

本工具提供了获取Spring MVC的@RequestMapping等注解中的路径信息的处理类，为com.adrninistrator.jacg.extensions.annotation_handler.SpringMvcRequestMappingHandler，该类已在method_annotation_handler.properties配置文件中指定

SpringMvcRequestMappingHandler类会获取类及方法上的@RequestMapping注解（或包含了该注解的其他注解）的路径信息，生成的注解信息格式为“@注解类名("/类注解中的path/方法注解中的path")”

```java
@Controller
@RequestMapping("test")
public class TestController {

    @RequestMapping(value = "test1", method = RequestMethod.POST)
    public void test1() {
        logger.info("");
    }
}
```

例如存在以上方法，则在生成的向上方法完整调用链中，TestController.test1()方法及相关的注解信息输出内容如下：

```
[0]#org.slf4j.Logger:info
[1]#  com.test.controller.TestController:test1@org.springframework.web.bind.annotation.RequestMapping("/test/test1")	(TestController:57)	!entry!
```

```java
@RestController
@RequestMapping("testrest2")
public class TestRest2Controller {

    @PostMapping(value = "post")
    @TestAttributeAnnotation
    public String post(HttpServletRequest httpRequest, @RequestBody final String req) {
        logger.info("");
    }
}
```

例如存在以上方法，则在生成的向上方法完整调用链中，TestRest2Controller.post()方法及相关的注解信息输出内容如下：

```
[0]#org.slf4j.Logger:info
[1]#  com.test.controller.TestRest2Controller:post@com.test.common.annotation.TestAttributeAnnotation@org.springframework.web.bind.annotation.PostMapping("/testrest2/post")	(TestRest2Controller:42)	!entry!
```
