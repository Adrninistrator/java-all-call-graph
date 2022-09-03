# 1. 更新说明

## 1.1. (0.6.3)

### 1.1.1. 支持使用本地文件数据库

支持使用本地文件形式的H2数据库，可不依赖外部的其他数据库，可在无法连接其他数据库（如MySQL）的环境中运行

H2数据库使用说明可参考[https://blog.csdn.net/a82514921/article/details/108029222](https://blog.csdn.net/a82514921/article/details/108029222)

本工具生成的H2数据库中，schema为“jacg”

### 1.1.2. 支持对目录进行处理

除了支持对jar/war包进行处理外，也支持对目录中的class、jar/war文件进行处理

支持指定一个或多个jar/war包，或一个或多个目录，或jar/war包与目录混合进行处理

该功能在java-callgraph2中实现，通过本工具的config.properties配置文件中的call.graph.jar.list参数进行配置

可参考[https://github.com/Adrninistrator/java-callgraph2](https://github.com/Adrninistrator/java-callgraph2)

### 1.1.3. 支持插件功能

提供用于生成Java方法UML时序图的插件功能

## 1.2. (0.6.7)

### 1.2.1. 增加及修改的配置文件

|增加或修改文件|文件路径|文件作用|
|---|---|---|
|增加|resources/_jacg_extensions/method_annotation_handler.properties|定义用于对方法上的注解进行处理的类完整类名|
|增加|resources/_jacg_sql/class_annotation.sql|用于保存类上的注解信息数据库表|
|修改|resources/_jacg_sql/method_annotation.sql|增加了保存注解属性的字段|

### 1.2.2. 提供处理方法上的注解信息的插件功能

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

### 1.2.3. 支持显示Spring MVC的@RequestMapping等注解中的路径信息

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

## 1.3. (0.7.0)

支持通过Java代码对参数配置进行设置，可覆盖配置文件中的参数（或仅使用Java代码中设置的参数，不使用配置文件中的参数）

可通过以下类的方法对参数配置进行设置

```java
com.adrninistrator.jacg.conf.ConfigureWrapper
```

在执行释放到项目中的test.jacg包中的入口类（如TestRunnerWriteDb），或执行jar包中com.adrninistrator.jacg.runner包中的入口类（如RunnerWriteDb）之前，需要先调用ConfigureWrapper类的方法设置参数配置

以下可参考`test.run_by_code`包中的测试代码，在`TestRunByCodeBase`类中调用了ConfigureWrapper类的方法

### 1.3.1. 设置_jacg_config/config.properties配置文件参数

```java
ConfigureWrapper.addConfig(ConfigKeyEnum configKeyEnum, String value);
```

`对于app.name参数，在以上方法中会将参数值中的-替换为_`

ConfigKeyEnum枚举类中定义了_jacg_config/config.properties配置文件中的参数key

通过value参数指定需要设置的参数值

示例如下：

```java
ConfigureWrapper.addConfig(ConfigKeyEnum.CKE_APPNAME, "test_rbc");
```

### 1.3.2. 设置_jacg_config、_jacg_extensions目录配置文件参数

```java
ConfigureWrapper.addOtherConfigSet(OtherConfigFileUseSetEnum otherConfigFileUseSetEnum, Set<String> configSet);
```

OtherConfigFileUseSetEnum枚举类中定义了_jacg_config目录中其他配置文件的文件名，以及_jacg_extensions目录中的配置文件名

通过configSet参数指定需要设置的Set类型的参数值

示例如下：

```java
ConfigureWrapper.addOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_IN_ALLOWED_CLASS_PREFIX, new HashSet<>(Arrays.asList(
        "test.call_graph.method_call",
        "test.call_graph.argument",
        "java.")));
```

### 1.3.3. 设置_jacg_find_keyword目录配置文件参数

```java
ConfigureWrapper.addOtherConfigList(OtherConfigFileUseListEnum otherConfigFileUseListEnum, List<String> configList);
```

OtherConfigFileUseListEnum枚举类中定义了_jacg_find_keyword目录中配置文件的文件名

通过configList参数指定需要设置的List类型的参数值

示例如下：

```java
ConfigureWrapper.addOtherConfigList(OtherConfigFileUseListEnum.OCFULE_FIND_KEYWORD_4CALLEE, Arrays.asList("!entry!", "<init>"));
```

## 1.4. (0.7.1)

- 支持人工添加缺失的方法调用关系（定制化代码开发）

请查看[extensions.md](extensions.md)，搜索“人工添加缺失的方法调用关系（定制化代码开发）”

## 1.5. (0.7.3)

### 1.5.1. 增加的配置文件

|文件路径|文件作用|
|---|---|
|resources/_jacg_sql/method_line_number.sql|方法代码行号信息表|

### 1.5.2. 支持指定方法名称生成向上方法调用链

在`_jacg_config/o_g4callee_class_name.properties`配置文件中，支持指定类名+方法名前缀，代表需要处理指定类的对应方法

格式如下：

```
[类名]:[方法名]
[类名]:[方法名+参数]
```

示例如下：

```
Test1:test
Test1:test(
Test1:test(java.lang.String)
```

配置文件`_jacg_config/config.properties`中的参数`gen.upwards.methods.file`不再使用

### 1.5.3. 支持指定代码行号生成向上方法调用链

在`_jacg_config/o_g4callee_class_name.properties`配置文件中，支持指定类名+代码行号，代表需要处理指定类的对应方法

格式如下：

```
[类名]:[代码行号]
```

[代码行号]可指定某个方法对应的任意代码行号，如C:f1()方法代码起止行号范围为[100,203]，则可指定以上范围的任意数字代表需要处理C:f1()方法

示例如下：

```
Test1:234
```

### 1.5.4. 支持指定代码行号生成向下方法调用链

在`_jacg_config/o_g4caller_entry_method.properties`配置文件中，支持指定类名+代码行号，代表需要处理指定类的对应方法

说明同上

### 1.5.5. 生成配置文件中的任务信息与结果文件的映射关系

每次生成方法调用链后，会在本次生成的目录中创建_mapping.txt文件，在该文件中记录了配置文件中的任务信息与结果文件的映射关系

该文件内容包含两列，以“\t”进行分隔，第1列为配置文件中指定的任务信息，第2列为生成结果文件路径，内容如下所示：

```
# 配置文件中指定的任务信息	生成结果文件路径
DbOperator:batchInsert(	_jacg_o_ee\20220505-211209.427\methods\DbOperator@batchInsert@PVuwu2XS1Fvxj_FQA1Ekog#056.txt
DbOperator:getInstance(	_jacg_o_ee\20220505-211209.427\methods\DbOperator@getInstance@Fg85cQ0J0brkEXpMPCoHUA#037.txt
DbOperator:268	_jacg_o_ee\20220505-211209.427\methods\DbOperator@batchInsert@PVuwu2XS1Fvxj_FQA1Ekog#056.txt
DbOperator:close(java.sql.Connection,java.sql.PreparedStatement)	_jacg_o_ee\20220505-211209.427\methods\DbOperator@close@9e5dsbPVD8648nV8on9Efw#05f.txt

RunnerGenAllGraph4Callee:101 101-101	_jacg_o_er\20220505-211230.131\RunnerGenAllGraph4Callee@doOperate@HommTjLUWABHR5l7RkDZkQ#043@101-101.txt
RunnerGenAllGraph4Callee:doOperate	_jacg_o_er\20220505-211230.131\RunnerGenAllGraph4Callee@doOperate@HommTjLUWABHR5l7RkDZkQ#043.txt
```

以上文件仅包含成功生成了调用链的任务及结果文件信息

假如在生成向上方法调用链时，在配置文件中指定了生成某个类的全部方法的调用链，也不会出现在以上文件中

## 1.6. (0.7.4)

生成向下完整方法调用链的配置文件`_jacg_config/o_g4caller_entry_method.properties`中，支持指定类名，代表需要处理对应类的所有方法

## 1.7. (0.7.5)

修复处理类或方法上注解信息时的bug

## 1.8. (0.7.7)

- 支持指定生成向下的调用链时是否忽略出现多次的被调用方法

该参数在以前的版本（0.6.0）已添加，但未进行说明

在配置文件`_jacg_config/o_g4caller_entry_method.properties`中增加了参数`ignore.dup.callee.in.one.caller`

生成向下的调用链时，在一个调用方法中出现多次的被调用方法（包含自定义数据），是否需要忽略，值为true/false

仅当开关为开时会忽略

默认值为关

- 支持不释放配置文件

尝试读取jar包中的配置文件，相关的配置文件可以不释放到项目中，可以通过Java代码对配置参数进行设置（进行二次开发时可能需要使用）。

- 支持指定存在多个实现类时是否当前文件中继续生成调用链

在配置文件`_jacg_config/o_g4caller_entry_method.properties`中增加了参数`multi.impl.gen.in.current.file`

生成向下的调用链时，若接口或父类存在多个实现类或子类，对于接口或父类方法调用多个实现类或子类方法的调用关系，是否需要在当前文件中继续生成，值为true/false

当开关为开时，以上调用关系会在当前文件中继续生成

当开关为关时，以上调用关系会在单独的目录中生成，目录名格式为“[接口或父类名]@[方法名]@[完整方法名HASH+长度]”，文件名格式为“[实现类或子类名]@[方法名]@[完整方法名HASH+长度].txt”；原始方法调用链对应的文件中，会记录当前方法调用接口或父类方法的调用关系，使用特殊的标记，格式为“!ext_data!JUMP_MULTI_IMPL@[接口或父类名]@[方法名]@[完整方法名HASH+长度]”

默认值为开

例如TestMulti.test1()方法中调用了Interface1接口的f1()方法，Interface1接口存在实现类ImplClass1、ImplClass2；

当以上开关为开时，Interface1.f1()方法调用ImplClass1.f1()、ImplClass2.f1()方法的调用关系会继续在TestMulti.test1()方法对应文件中生成；

当以上开关为关时，生成文件情况如下

TestMulti.test1()方法对应文件中调用Interface1.f1()方法的信息如下：

```
[1]#  [TestMulti:22]	test.call_graph.implement.Interface1:f1	!ext_data!JUMP_MULTI_IMPL@Interface1@f1@ix-_NHnAUilDstHxNyrtxQ#029
```

生成Interface1.f1()方法对应的目录，目录名为“Interface1@f1@ix-_NHnAUilDstHxNyrtxQ#029”

在以上目录中，分别生成ImplClass1.f1()、ImplClass2.f1()方法对应的保存调用链的文件，文件名为“ImplClass1@f1@28XJlqE5etyRh1WH_e_DLQ#029.txt”、“ImplClass2@f1@FixDUSOINEA0qji9Np3baA#029.txt”

- 支持指定配置文件路径

支持通过JVM参数"input.root.path"指定"_jacg_config"、"_jacg_extensions"、"_jacg_find_keyword"、"_jacg_sql"等配置文件目录所在的路径，参数结尾可不指定目录分隔符"/"或"\"

例如以上配置文件目录均在"C:/test"目录中，则可在JVM参数中通过以下方式指定

```
-Dinput.root.path=C:/test
```

- 支持处理没有被其他类调用的类

在以前的版本中，假如某个类的方法没有被其他类调用，则不支持为其生成向上或向下的方法完整调用链；在当前版本中进行了支持

- 方法循环调用Bug处理

在以前的版本中，生成向上或向下的方法完整调用链时，假如出现方法循环调用，在方法循环调用之后的方法调用可能不会生成出来；在当前版本进行了处理

- 支持指定指定批量写入数据库时每次插入的数量

支持通过JVM参数"db.insert.batch.size"指定批量向数据库写入数据时，每次执行插入操作时的记录数量；可用于查找出现重复的注解信息

- 超长注解属性进行截取

在向数据库注解信息表写入数据时，假如注解属性太长，会进行截止，最多保留3000字符，避免写入数据库失败

- 支持在MySQL中保留多个版本的表

在创建数据库表的sql语句中，将索引名称修改为增加了`app.name`参数（需要重新释放相关文件到项目中）

假如需要在MySQL中保留多个版本的数据库表，可在每次执行时使用不同的`app.name`参数，使创建的数据库表名不同，能够保留多个版本的表（以前的版本索引名称使用固定值，在MySQL中不能重复创建）

## 1.9. (0.7.8)

- 一些内部优化

对二次开发时进行优化与功能增加

- 支持人工向数据库方法调用表增加数据

只支持增加已经存在的类之间的方法调用

需要先禁止数据源自动关闭

- 根据关键字生成到起始方法的调用链优化

根据关键字生成到起始方法的调用链，减少生成文件耗时

- 注解属性处理优化

对类、方法上的注解属性进行处理时，支持全部类型的注解属性，包括数组、注解嵌套等使用方式

- 删除一些类

删除以下不需要使用的类

TestGenSingleCallGraph4ee

TestGenSingleCallGraph4er

GenSingleCallGraph

- 支持提取向上调用链的入口方法信息

支持获取指定方法的向上的调用链，并获取每个调用的对应的入口方法信息

- 修改目录开头的字符

由于字符“~”在Excel中会被显示为-，因此将目录开头的字符从“~”修改为“_”
