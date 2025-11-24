# 1. 静态分析生成 Java 代码方法完整调用链

## 1.1. 前言

在很多场景下，如果能够生成 Java 代码中方法之间的调用链，是很有帮助的，例如分析代码执行流程、确认被修改代码的影响范围、代码审计/漏洞分析等

IDEA 提供了显示调用指定 Java 方法向上的完整调用链的功能，可以通过“Navigate -> Call Hierarchy”菜单（快捷键：Ctrl+Alt+H） 使用；Eclipse 也提供了相同的功能。但以上都需要针对每个方法进行手工处理，不支持对方法进行过滤或者其他扩展功能

当前项目能够通过静态分析的方式批量生成指定 Java 方法向下的完整调用链，对于关注的 Java 方法，能够生成其向下调用的方法信息，及被调用方法再向下调用的方法，直到最下层被调用的方法

也可以生成调用指定 Java 类方法向上的完整调用链，对于关注的 Java 类的方法，能够生成调用对应方法的方法信息，及调用上述方法的信息，直到最上层未被其他方法调用的方法（通常是对外提供的服务，或定时任务等）

当前项目生成的 Java 方法完整调用链中，支持显示相关的包名、类名、方法名、方法参数、调用者源代码行号、方法注解、循环调用，入口方法

`当前项目提供了扩展功能，可用于为 Java 代码自动生成 UML 时序图`，可参考 [https://github.com/Adrninistrator/gen-java-code-uml-sequence-diagram](https://github.com/Adrninistrator/gen-java-code-uml-sequence-diagram)，根据关键字查找关注的方法时，可使用自定义 Java 代码判断是否满足关键字，在该文档中会有说明

## 1.2. 适用场景

### 1.2.1. 分析代码执行流程

使用当前项目生成指定方法向下调用链的功能，可以将代码中复杂的方法调用转换为相对简单的方法完整调用链形式展示

人工查看生成的调用链时，能够通过类名及方法名识别出对应含义

支持将不关注的方法调用忽略，仅展示重要的方法调用

对于分析代码执行流程有一定帮助，适合梳理交易流程、首次接触代码时熟悉流程等场景

### 1.2.2. 确认被修改代码的影响范围

使用当前项目生成指定方法向上调用链的功能，可以生成调用指定类的所有方法的调用链

能识别入口方法，减少人工逐层确认入口方法的工作量

可用于快速确认被修改代码的影响范围。

### 1.2.3. 应用功能拆分

在进行应用功能拆分时，需要准确定位指定功能涉及的数据库表，及使用了对应数据库表的相关入口方法

使用当前项目生成指定方法向下调用链的功能，生成指定入口方法向下的调用链，能够根据类的包名快速找到 Mapper 接口（使用 Mybatis 的场景），即可找到相关的数据库表

使用当前项目生成指定方法向上调用链的功能，生成调用指定 Mapper 接口向上的调用链，能够根据“!entry!”找到入口方法

重复执行以上过程，直到没有再找到新的 Mapper 接口（即数据库表）和入口方法，即可确认指定功能涉及的数据库表及相关入口方法

### 1.2.4. 代码审计/漏洞分析

在进行代码审计时，可使用当前项目梳理交易流程，生成指定方法向下的调用链，查找是否有调用敏感 API；或者生成指定方法向上的调用链，查找调用敏感 API 的场景

在进行漏洞分析时，结合当前项目生成的完整调用链辅助分析，也能提高效率

## 1.3. 输出结果示例

### 1.3.1. 从指定方法开始向上的完整调用链示例

从指定方法开始向上的完整调用链如下所示：

```
[0]#DestClass.destfunc()
[1]#  ClassA3.funcA3()	(ClassA3:10)
[2]#    ClassA2.funcA2()	(ClassA2:19)
[3]#      ClassA1.funcA1()	(ClassA1:23)    !entry!
[1]#  ClassB1.funcB1()	(ClassB1:57)    !entry!
[1]#  ClassC2.funcC2()	(ClassC2:31)
[2]#    ClassC1.funcC1()	(ClassC1:9)    !entry!
```

以上对应的调用关系如下所示：

![](https://gitee.com/adrninistrator/java-all-call-graph/raw/main/docs/pic/example-cg-4callee.png)

从指定方法开始向上的完整调用链输出结果格式类似一棵树，每行代表一个调用者 Java 方法，前面的数字越大代表调用层级越靠上，0 代表被调用的指定类中的方法

每行后部的“(TestClass:39)”格式的类名及数字代表当前调用者类名，及调用者方法对应的源代码行号

对于不被其他方法调用的方法，认为是调用链入口方法，在对应行的最后会显示“\t!entry!”

以下为使用当前项目生成的调用 Mybatis 的 MyBatisExceptionTranslator 类的部分方法向上完整调用链（方法参数太长，已省略）：

```
org.mybatis.spring.MyBatisExceptionTranslator:initExceptionTranslator()
[0]#org.mybatis.spring.MyBatisExceptionTranslator:initExceptionTranslator
[1]#  org.mybatis.spring.MyBatisExceptionTranslator:translateExceptionIfPossible	(MyBatisExceptionTranslator:88)	!entry!
[1]#  org.mybatis.spring.MyBatisExceptionTranslator:<init>	(MyBatisExceptionTranslator:72)
[2]#    org.mybatis.spring.MyBatisExceptionTranslator:<init>	(MyBatisExceptionTranslator:55)
[3]#      org.mybatis.spring.SqlSessionTemplate:<init>	(SqlSessionTemplate:106)
[4]#        org.mybatis.spring.SqlSessionTemplate:<init>	(SqlSessionTemplate:92)
[5]#          org.mybatis.spring.support.SqlSessionDaoSupport:createSqlSessionTemplate	(SqlSessionDaoSupport:70)
[6]#            org.mybatis.spring.support.SqlSessionDaoSupport:setSqlSessionFactory	(SqlSessionDaoSupport:52)	!entry!
[4]#        org.mybatis.spring.batch.MyBatisPagingItemReader:doReadPage	(MyBatisPagingItemReader:99)	!entry!
[4]#        org.mybatis.spring.batch.MyBatisBatchItemWriter:setSqlSessionFactory	(MyBatisBatchItemWriter:85)
[5]#          org.mybatis.spring.batch.builder.MyBatisBatchItemWriterBuilder:build	(MyBatisBatchItemWriterBuilder:113)	!entry!
```

`IDEA 使用技巧：在 IntelliJ IDEA 中，打开“Navigate Class...”窗口，即根据类名进入对应代码文件的窗口后，若输入 [类名]:[行号] 格式的内容并回车，可打开对应的代码文件并跳转到对应的行号`

### 1.3.2. 从指定方法开始向下完整调用链示例

从指定方法开始向下完整调用链如下所示：

```
[0]#SrcClass.srcfunc()
[1]#  [SrcClass:15]	ClassA1.funcA1()
[2]#    [ClassA1:27]	ClassA2a.funcA2a()
[2]#    [ClassA1:59]	ClassA2b.funcA2b()
[3]#      [ClassA2b:39]	ClassA3.funcA3()
[1]#  [SrcClass:17]	ClassB1.funcB1()
[1]#  [SrcClass:23]	ClassC1.funcC1()
[2]#    [ClassC1:75]	ClassC2.funcC2()
```

以上对应的调用关系如下所示：

![](https://gitee.com/adrninistrator/java-all-call-graph/raw/main/docs/pic/example-cg-4caller.png)

从指定方法开始向下完整调用链输出结果类似一棵树，每行代表一个被调用者 Java 方法，与实际的代码执行顺序一致，前面的数字越大代表调用层级越靠下，0 代表指定方法

每行前部的“\[TestClass:39\]”格式的类名及数字，代表当前调用者类名，及调用者方法及对应的源代码行号

以下为使用当前项目生成的 spring-webmvc 的 org.springframework.web.servlet.DispatcherServlet:doService() 方法向下的完整调用链如下，为了便于展示，省略了方法参数：

```
org.springframework.web.servlet.DispatcherServlet:doService
[0]#org.springframework.web.servlet.DispatcherServlet:doService
[1]#  [DispatcherServlet:927]	org.springframework.web.servlet.DispatcherServlet:logRequest	!no_callee!
[1]#  [DispatcherServlet:950]	org.springframework.web.servlet.FlashMapManager:retrieveAndUpdate@org.springframework.lang.Nullable	!no_callee!
[1]#  [DispatcherServlet:954]	org.springframework.web.servlet.FlashMap:<init>	!no_callee!
[1]#  [DispatcherServlet:965]	org.springframework.web.servlet.DispatcherServlet:doDispatch
[2]#    [DispatcherServlet:1044]	org.springframework.web.servlet.DispatcherServlet:checkMultipart
[3]#      [DispatcherServlet:1204]	org.springframework.web.servlet.DispatcherServlet:hasMultipartException	!no_callee!
[3]#      [DispatcherServlet:1210]	org.springframework.web.multipart.MultipartResolver:resolveMultipart	!no_callee!
[2]#    [DispatcherServlet:1050]	org.springframework.web.servlet.DispatcherServlet:noHandlerFound
[3]#      [DispatcherServlet:1287]	org.springframework.web.servlet.NoHandlerFoundException:<init>	!no_callee!
[2]#    [DispatcherServlet:1102]	org.springframework.web.servlet.HandlerExecutionChain:applyAfterConcurrentHandlingStarted
[3]#      [HandlerExecutionChain:195]	org.springframework.web.servlet.AsyncHandlerInterceptor:afterConcurrentHandlingStarted	!no_callee!
[2]#    [DispatcherServlet:1108]	org.springframework.web.servlet.DispatcherServlet:cleanupMultipart
[3]#      [DispatcherServlet:1251]	org.springframework.web.multipart.MultipartResolver:cleanupMultipart	!no_callee!
[2]#    [DispatcherServlet:1062]	org.springframework.web.context.request.ServletWebRequest:<init>	!no_callee!
[2]#    [DispatcherServlet:1062]	org.springframework.web.context.request.ServletWebRequest:checkNotModified
```

当方法上有注解时，也可以显示在结果中，格式为“@xxx”，如以上“@org.springframework.lang.Nullable”

当某个方法没有调用其他方法时，会显示标志“!no_callee!”

- 方法循环调用标志

当出现方法循环调用时，会显示出现循环调用的方法，格式为“!cycle\[x\]!”，x 代表循环调用的方法层级序号，对应的方法完整调用链示例如下

```
[7]#              [ServerRequest:71]	org.springframework.web.servlet.function.ServerRequest:methodName()
[8]#                [ServerRequest:0]	org.springframework.web.servlet.function.DefaultServerRequest:methodName()
[9]#                  [DefaultServerRequest:111]	org.springframework.web.servlet.function.DefaultServerRequest:servletRequest()
[10]#                    [DefaultServerRequest:149]	org.springframework.http.server.ServletServerHttpRequest:getServletRequest()	!no_callee!
[9]#                  [DefaultServerRequest:111]	javax.servlet.http.HttpServletRequest:getMethod()	!no_callee!
[8]#                [ServerRequest:0]	org.springframework.web.servlet.function.DefaultServerRequestBuilder$BuiltServerRequest:methodName()	!no_callee!
[8]#                [ServerRequest:0]	org.springframework.web.servlet.function.RequestPredicates$SubPathServerRequestWrapper:methodName()
[9]#                  [RequestPredicates$SubPathServerRequestWrapper:968]	org.springframework.web.servlet.function.ServerRequest:methodName()	!cycle[7]!
```

- Spring 事务中执行标志

当方法在 Spring 事务中执行时，包括使用@org.springframework.transaction.annotation.Transactional，或 org.springframework.transaction.support.TransactionTemplate:execute 方法，会在生成的调用链中指定 Spring 事务中执行标志“!run_in_spring_tx!”

```
test.callgraph.spring.tx.TestUseSpringTx:test()
[0]#test.callgraph.spring.tx.TestUseSpringTx:test()
[1]#  [TestUseSpringTx:11]	test.callgraph.spring.tx.TestSpringTx:<init>()
[2]#    [TestSpringTx:11]	java.lang.Object:<init>()	!no_callee!
[1]#  [TestUseSpringTx:12]	test.callgraph.spring.tx.TestSpringTx:test1()@org.springframework.transaction.annotation.Transactional	!run_in_spring_tx!
[2]#    [TestSpringTx:15]	java.lang.System:getProperty(java.lang.String)	!no_callee!

test.callgraph.innerclass.TestUseInnerClass:testTransactionCallbackWithoutResultInnerClass()
[0]#test.callgraph.innerclass.TestUseInnerClass:testTransactionCallbackWithoutResultInnerClass()
[1]#  [TestUseInnerClass:39]	org.springframework.transaction.support.TransactionTemplate:<init>()	!no_callee!
[1]#  [TestUseInnerClass:40]	test.callgraph.innerclass.TestUseInnerClass$2:<init>(test.callgraph.innerclass.TestUseInnerClass,java.lang.String)
[2]#    [TestUseInnerClass$2:40]	org.springframework.transaction.support.TransactionCallbackWithoutResult:<init>()	!no_callee!
[2]#    [TestUseInnerClass$2:0]	test.callgraph.innerclass.TestUseInnerClass$2:doInTransactionWithoutResult(org.springframework.transaction.TransactionStatus)	!run_in_spring_tx!
[3]#      [TestUseInnerClass$2:43]	java.lang.System:getProperty(java.lang.String)	!no_callee!
[3]#      [TestUseInnerClass$2:44]	java.io.PrintStream:println(java.lang.String)	!no_callee!
[1]#  [TestUseInnerClass:40]	org.springframework.transaction.support.TransactionTemplate:execute(org.springframework.transaction.support.TransactionCallback)	!no_callee!
```

## 1.4. 公共的使用说明

参考 [java-all-call-graph 使用说明](https://github.com/Adrninistrator/java-all-call-graph/blob/main/README.md)，“使用说明”部分

## 1.5. 前置处理

在使用当前功能前，需要首先解析指定的 Java 代码，并将结果写入数据库，参考 [对 Java 代码静态分析并支持 SQL 查询](parse_java_to_db.md)

## 1.6. 配置参数说明

### 1.6.1. 当前场景使用的配置参数

当前场景使用的配置参数使用说明，可参考 

[RunnerGenAllGraph4Callee - 生成指定方法向上的方法完整调用链](../class_use_config/RunnerGenAllGraph4Callee.md)

[RunnerGenAllGraph4Caller - 生成指定方法向下的方法完整调用链](../class_use_config/RunnerGenAllGraph4Caller.md)

### 1.6.2. 重要配置参数-java-all-call-graph

#### 1.6.2.1. _jacg_config/config_db.properties

指定需要使用的数据库信息

#### 1.6.2.2. _jacg_gen_all_call_graph/method_class_4callee.properties

指定需要生成向上的方法完整调用链的类或方法

#### 1.6.2.3. _jacg_gen_all_call_graph/method_class_4caller.properties

指定需要生成向下的方法完整调用链的类或方法

### 1.6.3. 配置参数示例

参考 [配置参数示例](../_jacg_all_config.md)

### 1.6.4. EL 表达式使用通用说明文档

参考 [EL 表达式使用通用说明文档](../../java-all-call-graph/src/main/resources/_el_example/el_usage.md)

### 1.6.5. EL 表达式字符串比较说明文档

参考 [EL 表达式字符串比较说明文档](../../java-all-call-graph/src/main/resources/_el_example/string_compare.md)

## 1.7. 入口类

### 1.7.1. 生成向上的方法完整调用链

- 通过配置文件指定配置参数

当通过配置文件指定配置参数时，入口类为 test.jacg.TestRunnerGenAllGraph4Callee

该类会调用下面的类

- 通过代码指定配置参数

通过代码指定配置参数时，入口类为 com.adrninistrator.jacg.runner.RunnerGenAllGraph4Callee

### 1.7.2. 生成向下的方法完整调用链

- 通过配置文件指定配置参数

当通过配置文件指定配置参数时，入口类为 test.jacg.TestRunnerGenAllGraph4Caller

该类会调用下面的类

- 通过代码指定配置参数

通过代码指定配置参数时，入口类为 com.adrninistrator.jacg.runner.RunnerGenAllGraph4Caller

## 1.8. 示例代码

以下为通过代码指定配置参数执行时的示例代码

### 1.8.1. 生成向上的方法完整调用链

```java
ConfigureWrapper configureWrapper = new ConfigureWrapper();
configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLEE, System.class.getName());

Assert.assertTrue(new RunnerGenAllGraph4Callee(configureWrapper).run());
```

对应 test.runbycodemain.TestRBCRunnerGenAllGraph4Callee#testExampleWriteToFile 方法

### 1.8.2. 生成向下的方法完整调用链

```java
ConfigureWrapper configureWrapper = new ConfigureWrapper();
configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER, TestMCCaller.class.getName());

Assert.assertTrue(new RunnerGenAllGraph4Caller(configureWrapper).run());
```

对应 test.runbycodemain.TestRBCRunnerGenAllGraph4Caller#testExampleWriteToFile 方法

## 1.9. 支持的功能

### 1.9.1. 循环调用处理

在生成方法完整调用链时，如果遇到循环调用的方法，会显示对应的标志 cycle，不会循环生成，详情可参考示例

### 1.9.2. 方法完整调用链入口方法

在生成向上的方法完整调用链时，如果找到了没有被其他方法调用的方法，认为是方法完整调用链入口方法，会显示对应的标志！entry!，详情可参考示例

### 1.9.3. 方法完整调用链最底层方法

在生成向下的方法完整调用链时，如果找到了没有调用其他方法的方法，认为是方法完整调用链最底层方法，会显示对应的标志！no_callee!，详情可参考示例

### 1.9.4. Spring 事务中执行的方法

在生成向下的方法完整调用链时，如果找到了在 Spring 事务中执行的方法，会显示对应的标志“!run_in_spring_tx!”，详情可参考示例

### 1.9.5. 指定生成位置

在生成方法完整调用链时，支持输出到文件，默认生成在当前目录的 _jacg_o_ee、_jacg_o_er 目录中，分别对应向上与向下的方法完整调用链；支持通过配置参数修改生成的根目录与子目录名

生成方法完整调用链也支持在内存中返回

生成向上的方法完整调用链时，获取内存中返回的调用链使用的方法，与调用链结构分别如下：

```java
com.adrninistrator.jacg.runner.RunnerGenAllGraph4Callee:getAllMethodCallLineData4EeMap

Map<String, List<MethodCallLineData4Ee>>
```

生成向下的方法完整调用链时，获取内存中返回的调用链使用的方法，与调用链结构分别如下：

```java
com.adrninistrator.jacg.runner.RunnerGenAllGraph4Caller:getAllMethodCallLineData4ErMap

Map<String, List<MethodCallLineData4Er>>
```

### 1.9.6. 指定生成方法详细程度

支持指定四种详细程度：

```
0: 最详细 完整类名+方法名+方法参数+返回类型
1: 详细 完整类名+方法名+方法参数
2: 中等 完整类名+方法名
3: 最简单 简单类名（对于同名类展示完整类名）+方法名
```

### 1.9.7. 指定生成调用链深度与数量

支持指定生成调用链的深度与数量，当达到允许的最大深度或数量时，会停止生成调用链，默认无限制

### 1.9.8. 忽略部分方法调用

支持通过 EL 表达式配置生成调用链时忽略部分方法调用，使用方式可查看 EL 表达式使用通用说明文档

### 1.9.9. 处理多态-父类调用子类实现/重写的方法

对于涉及多态的父类调用子类实现/重写的方法的场景，会进行专门处理，以保证生成的调用链准确

需要处理的场景为：被调用的对象为子类，被调用方法在父类中实现，父类的方法调用了子类中重写的方法

例如以下代码中，父类 ChildOverrideSuper 中有 exec 方法，会调用抽象方法 custom，custom 方法需要在子类中实现

在子类 ChildOverrideChildA 中，重写了 custom 方法

在 TestUseChildOverride 类的 test1 方法中，调用了 ChildOverrideChildA 对象的 exec 方法，即父类 ChildOverrideSuper 的 exec 方法

```java
public abstract class ChildOverrideSuper {
    public String exec() {
        custom();
        return "";
    }

    protected abstract void custom();
}

public class ChildOverrideChildA extends ChildOverrideSuper {
    @Override
    protected void custom() {
        printA();
    }

    private void printA() {
        System.out.println("a");
    }
}

public class TestUseChildOverride {
    public void test1() {
        ChildOverrideSuper childOverrideChildA = new ChildOverrideChildA();
        childOverrideChildA.exec();
    }
}
```

对于 TestUseChildOverride:test1 方法，生成的向下的方法完整调用链如下，可以看到能够找到调用子类重写的方法 ChildOverrideChildA:custom，与实际的方法调用情况一致：

```
test.callgraph.polymorphism.childoverride.TestUseChildOverride:test1()
[0]#test.callgraph.polymorphism.childoverride.TestUseChildOverride:test1()
[1]#  [TestUseChildOverride:13]	test.callgraph.polymorphism.childoverride.ChildOverrideChildA:<init>()
[2]#    [ChildOverrideChildA:8]	test.callgraph.polymorphism.childoverride.ChildOverrideSuper:<init>()
[3]#      [ChildOverrideSuper:8]	java.lang.Object:<init>()	!no_callee!
[1]#  [TestUseChildOverride:14]	test.callgraph.polymorphism.childoverride.ChildOverrideChildA:exec()
[2]#    [ChildOverrideChildA:0]	test.callgraph.polymorphism.childoverride.ChildOverrideSuper:exec()
[3]#      [ChildOverrideSuper:10]	test.callgraph.polymorphism.childoverride.ChildOverrideChildA:custom()
[4]#        [ChildOverrideChildA:12]	test.callgraph.polymorphism.childoverride.ChildOverrideChildA:printA()
[5]#          [ChildOverrideChildA:16]	java.io.PrintStream:println(java.lang.String)	!no_callee!
```

对应的示例代码参考 test.callgraph.polymorphism.childoverride.TestUseChildOverride 类

### 1.9.10. 处理多态-方法参数作为被调用对象

对于涉及多态的以方法参数作为被调用对象的场景，需要进行专门处理，才能保证生成的调用链准确

需要处理的场景为：在某个方法中，调用了方法参数对象的方法，该方法定义的方法参数为父类类型，外部其他方法调用该方法时，传入的实际参数为子类

#### 1.9.10.1. 示例代码

示例代码如下

接口 CalleeArgTypePolymorphismInterface1 中有方法 cmd1、cmd2

公共方法 CalleeArgTypePolymorphismTool1:run0 方法的参数 2 类型为以上接口 CalleeArgTypePolymorphismInterface1，会调用参数 2 的 cmd1、cmd2 方法

CalleeArgTypePolymorphismChild1B 为 CalleeArgTypePolymorphismInterface1 接口的实现类

CalleeArgTypePolymorphismService1:testRun0Use1B 方法中调用了 CalleeArgTypePolymorphismTool1:run0 方法，使用的参数 2 类型为子类 CalleeArgTypePolymorphismChild1B

```java
public interface CalleeArgTypePolymorphismInterface1 {
    void cmd1();
    void cmd2();
}

public class CalleeArgTypePolymorphismTool1 {
    public void run0(String str1, CalleeArgTypePolymorphismInterface1 arg1, String str2) {
        arg1.cmd1();
        arg1.cmd2();
    }
}

public class CalleeArgTypePolymorphismChild1B implements CalleeArgTypePolymorphismInterface1 {
    @Override
    public void cmd1() {
        System.getProperty("");
    }

    @Override
    public void cmd2() {
        System.setErr(null);
    }
}

public class CalleeArgTypePolymorphismService1 {
    public void testRun0Use1B() {
        CalleeArgTypePolymorphismTool1 calleeArgTypePolymorphismTool1 = new CalleeArgTypePolymorphismTool1();
        CalleeArgTypePolymorphismInterface1 calleeArgTypePolymorphismChild1B = new CalleeArgTypePolymorphismChild1B();
        calleeArgTypePolymorphismTool1.run0("a", calleeArgTypePolymorphismChild1B, "b");
    }
}
```

#### 1.9.10.2. 生成的调用链示例

对于以上示例代码，生成的调用链示例如下

可以看到识别到 CalleeArgTypePolymorphismTool1:run0 方法调用了 CalleeArgTypePolymorphismChild1B:cmd1、cmd2 方法，与代码执行时实际会调用的方法一致

```
test.callgraph.polymorphism.calleeargtype.CalleeArgTypePolymorphismService1:testRun0Use1B()
[0]#test.callgraph.polymorphism.calleeargtype.CalleeArgTypePolymorphismService1:testRun0Use1B()
[1]#  [CalleeArgTypePolymorphismService1:29]	test.callgraph.polymorphism.calleeargtype.CalleeArgTypePolymorphismTool1:<init>()
[2]#    [CalleeArgTypePolymorphismTool1:14]	java.lang.Object:<init>()	!no_callee!
[1]#  [CalleeArgTypePolymorphismService1:30]	test.callgraph.polymorphism.calleeargtype.superchild.CalleeArgTypePolymorphismChild1B:<init>()
[2]#    [CalleeArgTypePolymorphismChild1B:8]	java.lang.Object:<init>()	!no_callee!
[1]#  [CalleeArgTypePolymorphismService1:31]	test.callgraph.polymorphism.calleeargtype.CalleeArgTypePolymorphismTool1:run0(java.lang.String,test.callgraph.polymorphism.calleeargtype.superchild.CalleeArgTypePolymorphismInterface1,java.lang.String)
[2]#    [CalleeArgTypePolymorphismTool1:19]	test.callgraph.polymorphism.calleeargtype.superchild.CalleeArgTypePolymorphismChild1B:cmd1()
[3]#      [CalleeArgTypePolymorphismChild1B:12]	java.lang.System:getProperty(java.lang.String)	!no_callee!
[2]#    [CalleeArgTypePolymorphismTool1:20]	test.callgraph.polymorphism.calleeargtype.superchild.CalleeArgTypePolymorphismChild1B:cmd2()
[3]#      [CalleeArgTypePolymorphismChild1B:17]	java.lang.System:setErr(java.io.PrintStream)	!no_callee!
```

#### 1.9.10.3. 配置参数指定方式

对于涉及多态的以方法参数作为被调用对象的场景，需要在配置参数中进行配置

在配置文件 _jacg_gen_all_call_graph/caller_graph_callee_arg_type_polymorphism.properties 指定哪些方法参数作为被调用对象涉及多态时的类型替换，或在代码中进行配置，具体格式可参考 [配置参数示例](../_jacg_all_config.md)

可参考示例方法 test.runbycode.callgraph.calleeargtypepolymorphism.TestGenCallerGraphCalleeArgTypePolymorphism:testRun0Use1B，在代码中设置的参数如下

```java
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_CALLER_GRAPH_CALLEE_ARG_TYPE_POLYMORPHISM,
                "test.callgraph.polymorphism.calleeargtype.CalleeArgTypePolymorphismTool1:run0(java.lang.String,test.callgraph.polymorphism.calleeargtype.superchild.CalleeArgTypePolymorphismInterface1,java.lang.String)=2"
        );
```

### 1.9.11. 生成调用链时显示方法注解

多个，“@xxx@yyy”

#### 1.9.11.3. 通过配置参数指定方法注解显示方式

_jacg_extensions/method_annotation_formatter.properties

com.adrninistrator.jacg.annotation.formatter.AbstractAnnotationFormatter

```
com.adrninistrator.jacg.annotation.formatter.SpringMvcRequestMappingFormatter
com.adrninistrator.jacg.annotation.formatter.SpringTransactionalFormatter
com.adrninistrator.jacg.annotation.formatter.DefaultAnnotationFormatter
com.adrninistrator.jacg.annotation.formatter.HideAnnotationFormatter
```

#### 

- 示例代码

```java
public class MethodWithAnnotation extends TestParentClass1 {

    @TestAnnotation(
            strValue = "bbb\r\n",
            intValue = 222,
            intArrayValue = {11, 12, 13, 14},
            clazz1 = DbOperWrapper.class,
            enum1 = ConfigKeyEnum.CKE_CALL_GRAPH_OUTPUT_DETAIL,
            annotation1 = @TestAnnotationInner(valueA = "va1", valueB = "vb1"))
    public void test1() {
```

- 生成的调用链示例

```
[0]#test.callgraph.annotation.MethodWithAnnotation:test1()@test.callgraph.annotation.TestAnnotation
```

#### 

- 示例代码

```java

```

- 生成的调用链示例

```
[1]#  [TestUseSpringTx:12]	test.callgraph.spring.tx.TestSpringTx:test1()@org.springframework.transaction.annotation.Transactional	!run_in_spring_tx!

[1]#  [TestUseSpringTx:13]	test.callgraph.spring.tx.TestSpringTx:test2()@org.springframework.transaction.annotation.Transactional(propagation=REQUIRES_NEW)	!run_in_spring_tx!
```

#### 

- 示例代码

```java

```

- 生成的调用链示例

```
[0]#test.callgraph.spring.mvc.TestSpringController1:get1()@org.springframework.web.bind.annotation.GetMapping(/test1/get1)@org.springframework.web.bind.annotation.ResponseBody
```

### 1.9.12. 生成调用链时显示业务功能数据

## 1.10. 无法正确处理的情况

以下情况，对应的方法找不到被调用关系，可能会被误识别为入口方法：

- 不是直接通过 Java 方法进行调用的情况（例如在 XML 文件中配置代码执行流程、通过注解配置代码执行流程、使用 AOP 处理等）
- 未被调用的方法

## 1.11. 多余的调用关系处理

### 1.11.1. 问题

当代码中引入了接口或抽象父类，且对应多个实现类或子类时，生成的方法完整调用链可能存在多余的调用关系。

当一个接口对应多个实现类时，若在某个类中引入了接口，并调用其方法，生成的完整调用链中，可能将当前类未使用的其他实现类相关的调用关系也包含进来；

当一个抽象父类对应多个非抽象子类时，若在某个类中引入了抽象父类，并调用其方法，生成的完整调用链中，可能将当前类未使用的其他非抽象子类相关的调用关系也包含进来。

当代码中使用工厂模式获取某个接口/抽象父类的实现类/非抽象子类时，也可能会出现类似的问题。

### 1.11.2. 解决

当存在以上情况时，当前项目会在当前目录生成“_notice_multi_ITF.md”或“_notice_multi_SCC.md”文件，可按照文档中的提示，将前缀为“method\_call\_”的数据库表中不需要的方法调用设置为禁用，或者在生成调用链时通过 EL 表达式指定需要忽略哪些方法

当需要将禁用的方法调用恢复为启用时，可按照当前目录生成的“_notice_disabled_ITF.md”或“_notice_disabled_SCC.md”文件的说明进行操作
