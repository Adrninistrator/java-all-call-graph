# 1. 更新说明

## 1.1. (0.6.3)

### 1.1.1. 支持使用本地文件数据库

支持使用本地文件形式的 H2 数据库，可不依赖外部的其他数据库，可在无法连接其他数据库（如 MySQL）的环境中运行

H2 数据库使用说明可参考 [https://blog.csdn.net/a82514921/article/details/108029222](https://blog.csdn.net/a82514921/article/details/108029222)

本工具生成的 H2 数据库中，schema 为“jacg”

### 1.1.2. 支持对目录进行处理

除了支持对 jar/war 包进行处理外，也支持对目录中的 class、jar/war 文件进行处理

支持指定一个或多个 jar/war 包，或一个或多个目录，或 jar/war 包与目录混合进行处理

该功能在 java-callgraph2 中实现，通过本工具的 config.properties 配置文件中的 call.graph.jar.list 参数进行配置

可参考 [https://github.com/Adrninistrator/java-callgraph2](https://github.com/Adrninistrator/java-callgraph2)

### 1.1.3. 支持插件功能

提供用于生成 Java 方法 UML 时序图的插件功能

## 1.2. (0.6.7)

### 1.2.1. 增加及修改的配置文件

|增加或修改文件|文件路径|文件作用|
|---|---|---|
|增加|resources/_jacg_extensions/method_annotation_handler.properties|定义用于对方法上的注解进行处理的类完整类名|
|增加|resources/_jacg_sql/class_annotation.sql|用于保存类上的注解信息数据库表|
|修改|resources/_jacg_sql/method_annotation.sql|增加了保存注解属性的字段|

### 1.2.2. 提供处理方法上的注解信息的插件功能

在 method_annotation_handler.properties 配置文件中，可以定义用于对方法上的注解进行处理的类完整类名，该文件每行指定一项配置，可指定多行

对方法上的注解进行处理的类需要继承自 com.adrninistrator.jacg.extensions.annotation_handler.AbstractAnnotationHandler，并实现以下方法

|方法名|方法作用|
|---|---|
|checkHandleAnnotation|判断当前类是否处理对应的注解|
|handleAnnotation|返回方法上的注解处理后的结果|

本工具在生成方法完整调用链时，会先遍历 method_annotation_handler.properties 配置文件中指定所有的 AbstractAnnotationHandler 子类，即对方法上的注解进行处理的类，调用 checkHandleAnnotation 判断当前类是否会处理的注解，若是则调用 handleAnnotation 方法获取处理后的注解信息

最后会调用默认的方法注解处理类 com.adrninistrator.jacg.extensions.annotation_handler.DefaultAnnotationHandler 进行处理，该类会处理所有的注解，生成的注解信息格式为“@注解类名”，例如“@org.aspectj.lang.annotation.Around”

`假如一个方法上存在多个注解，则每个注解的信息会按照注解类名升序排序后，依次拼接在方法信息后`

DefaultAnnotationHandler 类不需要在 method_annotation_handler.properties 配置文件中指定

### 1.2.3. 支持显示 Spring MVC 的@RequestMapping 等注解中的路径信息

本工具提供了获取 Spring MVC 的@RequestMapping 等注解中的路径信息的处理类，为 com.adrninistrator.jacg.extensions.annotation_handler.SpringMvcRequestMappingHandler，该类已在 method_annotation_handler.properties 配置文件中指定

SpringMvcRequestMappingHandler 类会获取类及方法上的@RequestMapping 注解（或包含了该注解的其他注解）的路径信息，生成的注解信息格式为“@注解类名 ("/类注解中的 path/方法注解中的 path")”

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

例如存在以上方法，则在生成的向上方法完整调用链中，TestController.test1() 方法及相关的注解信息输出内容如下：

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

例如存在以上方法，则在生成的向上方法完整调用链中，TestRest2Controller.post() 方法及相关的注解信息输出内容如下：

```
[0]#org.slf4j.Logger:info
[1]#  com.test.controller.TestRest2Controller:post@com.test.common.annotation.TestAttributeAnnotation@org.springframework.web.bind.annotation.PostMapping("/testrest2/post")	(TestRest2Controller:42)	!entry!
```

## 1.3. (0.7.0)

支持通过 Java 代码对参数配置进行设置，可覆盖配置文件中的参数（或仅使用 Java 代码中设置的参数，不使用配置文件中的参数）

可通过以下类的方法对参数配置进行设置

```java
com.adrninistrator.jacg.conf.ConfigureWrapper
```

在执行释放到项目中的 test.jacg 包中的入口类（如 TestRunnerWriteDb），或执行 jar 包中 com.adrninistrator.jacg.runner 包中的入口类（如 RunnerWriteDb）之前，需要先调用 ConfigureWrapper 类的方法设置参数配置

以下可参考`test.run_by_code`包中的测试代码，在`TestRunByCodeBase`类中调用了 ConfigureWrapper 类的方法

### 1.3.1. 设置_jacg_config/config.properties 配置文件参数

```java
ConfigureWrapper.addConfig(ConfigKeyEnum configKeyEnum, String value);
```

`对于 app.name 参数，在以上方法中会将参数值中的-替换为_`

ConfigKeyEnum 枚举类中定义了_jacg_config/config.properties 配置文件中的参数 key

通过 value 参数指定需要设置的参数值

示例如下：

```java
ConfigureWrapper.addConfig(ConfigKeyEnum.CKE_APPNAME, "test_rbc");
```

### 1.3.2. 设置_jacg_config、_jacg_extensions 目录配置文件参数

```java
ConfigureWrapper.addOtherConfigSet(OtherConfigFileUseSetEnum otherConfigFileUseSetEnum, Set<String> configSet);
```

OtherConfigFileUseSetEnum 枚举类中定义了_jacg_config 目录中其他配置文件的文件名，以及_jacg_extensions 目录中的配置文件名

通过 configSet 参数指定需要设置的 Set 类型的参数值

示例如下：

```java
ConfigureWrapper.addOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_IN_ALLOWED_CLASS_PREFIX, new HashSet<>(Arrays.asList(
        "test.call_graph.method_call",
        "test.call_graph.argument",
        "java.")));
```

### 1.3.3. 设置_jacg_find_keyword 目录配置文件参数

```java
ConfigureWrapper.addOtherConfigList(OtherConfigFileUseListEnum otherConfigFileUseListEnum, List<String> configList);
```

OtherConfigFileUseListEnum 枚举类中定义了_jacg_find_keyword 目录中配置文件的文件名

通过 configList 参数指定需要设置的 List 类型的参数值

示例如下：

```java
ConfigureWrapper.addOtherConfigList(OtherConfigFileUseListEnum.OCFULE_FIND_KEYWORD_4CALLEE, Arrays.asList("!entry!", "<init>"));
```

## 1.4. (0.7.1)

- 支持人工添加缺失的方法调用关系（定制化代码开发）

请查看 [extensions.md](extensions.md)，搜索“人工添加缺失的方法调用关系（定制化代码开发）”

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

[代码行号] 可指定某个方法对应的任意代码行号，如 C:f1() 方法代码起止行号范围为 [100,203]，则可指定以上范围的任意数字代表需要处理 C:f1() 方法

示例如下：

```
Test1:234
```

### 1.5.4. 支持指定代码行号生成向下方法调用链

在`_jacg_config/o_g4caller_entry_method.properties`配置文件中，支持指定类名+代码行号，代表需要处理指定类的对应方法

说明同上

### 1.5.5. 生成配置文件中的任务信息与结果文件的映射关系

每次生成方法调用链后，会在本次生成的目录中创建_mapping.txt 文件，在该文件中记录了配置文件中的任务信息与结果文件的映射关系

该文件内容包含两列，以“\t”进行分隔，第 1 列为配置文件中指定的任务信息，第 2 列为生成结果文件路径，内容如下所示：

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

生成向下方法完整调用链的配置文件`_jacg_config/o_g4caller_entry_method.properties`中，支持指定类名，代表需要处理对应类的所有方法

## 1.7. (0.7.5)

修复处理类或方法上注解信息时的 bug

## 1.8. (0.7.7)

- 支持指定生成向下的调用链时是否忽略出现多次的被调用方法

该参数在以前的版本（0.6.0）已添加，但未进行说明

在配置文件`_jacg_config/o_g4caller_entry_method.properties`中增加了参数`ignore.dup.callee.in.one.caller`

生成向下的调用链时，在一个调用方法中出现多次的被调用方法（包含自定义数据），是否需要忽略，值为 true/false

仅当开关为开时会忽略

默认值为关

- 支持不释放配置文件

尝试读取 jar 包中的配置文件，相关的配置文件可以不释放到项目中，可以通过 Java 代码对配置参数进行设置（进行二次开发时可能需要使用）

- 支持指定存在多个实现类时是否当前文件中继续生成调用链

在配置文件`_jacg_config/o_g4caller_entry_method.properties`中增加了参数`multi.impl.gen.in.current.file`

生成向下的调用链时，若接口或父类存在多个实现类或子类，对于接口或父类方法调用多个实现类或子类方法的调用关系，是否需要在当前文件中继续生成，值为 true/false

当开关为开时，以上调用关系会在当前文件中继续生成

当开关为关时，以上调用关系会在单独的目录中生成，目录名格式为“[接口或父类名]@[方法名]@[完整方法名 HASH+长度]”，文件名格式为“[实现类或子类名]@[方法名]@[完整方法名 HASH+长度].txt”；原始方法调用链对应的文件中，会记录当前方法调用接口或父类方法的调用关系，使用特殊的标记，格式为“!ext_data!JUMP_MULTI_IMPL@[接口或父类名]@[方法名]@[完整方法名 HASH+长度]”

默认值为开

例如 TestMulti.test1() 方法中调用了 Interface1 接口的 f1() 方法，Interface1 接口存在实现类 ImplClass1、ImplClass2；

当以上开关为开时，Interface1.f1() 方法调用 ImplClass1.f1()、ImplClass2.f1() 方法的调用关系会继续在 TestMulti.test1() 方法对应文件中生成；

当以上开关为关时，生成文件情况如下

TestMulti.test1() 方法对应文件中调用 Interface1.f1() 方法的信息如下：

```
[1]#  [TestMulti:22]	test.call_graph.implement.Interface1:f1	!ext_data!JUMP_MULTI_IMPL@Interface1@f1@ix-_NHnAUilDstHxNyrtxQ#029
```

生成 Interface1.f1() 方法对应的目录，目录名为“Interface1@f1@ix-_NHnAUilDstHxNyrtxQ#029”

在以上目录中，分别生成 ImplClass1.f1()、ImplClass2.f1() 方法对应的保存调用链的文件，文件名为“ImplClass1@f1@28XJlqE5etyRh1WH_e_DLQ#029.txt”、“ImplClass2@f1@FixDUSOINEA0qji9Np3baA#029.txt”

- 支持指定配置文件路径

支持通过 JVM 参数"input.root.path"指定"_jacg_config"、"_jacg_extensions"、"_jacg_find_keyword"、"_jacg_sql"等配置文件目录所在的路径，参数结尾可不指定目录分隔符"/"或"\"

例如以上配置文件目录均在"C:/test"目录中，则可在 JVM 参数中通过以下方式指定

```
-Dinput.root.path=C:/test
```

- 支持处理没有被其他类调用的类

在以前的版本中，假如某个类的方法没有被其他类调用，则不支持为其生成向上或向下的方法完整调用链；在当前版本中进行了支持

- 方法循环调用 Bug 处理

在以前的版本中，生成向上或向下的方法完整调用链时，假如出现方法循环调用，在方法循环调用之后的方法调用可能不会生成出来；在当前版本进行了处理

- 支持指定指定批量写入数据库时每次插入的数量

支持通过 JVM 参数"db.insert.batch.size"指定批量向数据库写入数据时，每次执行插入操作时的记录数量；可用于查找出现重复的注解信息

- 超长注解属性进行截取

在向数据库注解信息表写入数据时，假如注解属性太长，会进行截止，最多保留 3000 字符，避免写入数据库失败

- 支持在 MySQL 中保留多个版本的表

在创建数据库表的 sql 语句中，将索引名称修改为增加了`app.name`参数（需要重新释放相关文件到项目中）

假如需要在 MySQL 中保留多个版本的数据库表，可在每次执行时使用不同的`app.name`参数，使创建的数据库表名不同，能够保留多个版本的表（以前的版本索引名称使用固定值，在 MySQL 中不能重复创建）

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

由于字符“\~”在 Excel 中会被显示为-，因此将目录开头的，以及其他配置文件中的字符从“\~”修改为“_”

## 1.10. (0.7.9)

- 一些内部优化

注解处理类、数据库操作类增加方法

## 1.11. (0.8.0)

- 支持每次执行任务使用独立的配置信息

支持通过 Java 代码调用时，每次执行任务使用独立的配置信息（ConfigureWrapper 类变成非静态方式使用），可支持多个任务并行执行（适用于在 Web 项目中使用 java-all-call-graph 的场景）

## 1.12. (1.0.1)

### 1.12.1. 支持获取方法调用中使用的参数值、被调用对象类型等

参数信息在数据库表`method_call_info`

假如需要使方法完整调用链文件中显示方法调用的参数信息，可修改配置文件`_jacg_config/config.properties`，指定`caller.show.raw.method.call.info`参数值为 true

在方法完整调用链最后一列（使用"\t"分隔），为方法调用中使用的参数值、被调用对象类型等信息，以`!ext_data!method_call_info@`开头，为 JSON 字符串格式，各个 key 说明如下：

|key|含义|
|---|---|
|obj|被调用对象的信息|
|args|参数的信息|
|t|可能的类型列表|
|v|可能的值列表|
|sf|可能的静态字段列表|
|sfm|可能的静态字段的方法列表|

#### 1.12.1.1. 示例——参数值、类型、静态字段

- 代码

```java
public static void test() {
    logger.info("test\ra");
    System.out.println("test\ra");
    System.out.println("test\nb");
    System.out.println("test\r\nc");
    System.out.println("test\tc");
    testBigDecimal(BigDecimal.ONE);
    ...
}

public static void testBigDecimal(BigDecimal arg) {
    System.out.println(arg);
}
```

- 解析结果

```java
[0]#test.call_graph.argument.TestArgument1:test()
[1]#  [TestArgument1:18]	java.io.PrintStream:println(java.lang.String)	!ext_data!method_call_info@{"obj":{"sf":["java.lang.System:out"]},"args":{"0":{"v":["test\ra"]}}}
[1]#  [TestArgument1:19]	java.io.PrintStream:println(java.lang.String)	!ext_data!method_call_info@{"obj":{"sf":["java.lang.System:out"]},"args":{"0":{"v":["test\nb"]}}}
[1]#  [TestArgument1:20]	java.io.PrintStream:println(java.lang.String)	!ext_data!method_call_info@{"obj":{"sf":["java.lang.System:out"]},"args":{"0":{"v":["test\r\nc"]}}}
[1]#  [TestArgument1:21]	java.io.PrintStream:println(java.lang.String)	!ext_data!method_call_info@{"obj":{"sf":["java.lang.System:out"]},"args":{"0":{"v":["test\tc"]}}}
[1]#  [TestArgument1:22]	test.call_graph.argument.TestArgument1:testBigDecimal(java.math.BigDecimal)	!ext_data!method_call_info@{"args":{"0":{"sf":["java.math.BigDecimal:ONE"]}}}
[2]#    [TestArgument1:37]	java.io.PrintStream:println(java.lang.Object)	!ext_data!method_call_info@{"obj":{"sf":["java.lang.System:out"]},"args":{"0":{"t":["java.math.BigDecimal"]}}}
```

#### 1.12.1.2. 示例——所有可能的多种参数值

- 代码

```java
int i = (int) System.currentTimeMillis() % 10;
String s1 = (i == 7 ? "a" : "b");

String s2 = "aa";
int ii = 1;
if (i == 7) {
    s2 = "bb";
}

testString(s1);
testString(s2);
```

- 解析结果

```java
[0]#test.call_graph.argument.TestArgument2:test()
[1]#  [TestArgument2:10]	java.lang.System:currentTimeMillis()
[1]#  [TestArgument2:19]	test.call_graph.argument.TestArgument2:testString(java.lang.String)	!ext_data!method_call_info@{"args":{"0":{"v":["a","b"]}}}
[1]#  [TestArgument2:20]	test.call_graph.argument.TestArgument2:testString(java.lang.String)	!ext_data!method_call_info@{"args":{"0":{"v":["bb","aa"]}}}
```

### 1.12.2. 依赖组件

- 数据源修改

使用的数据源由 c3p0 修改为 druid

- 新增依赖组件

com.github.adrninistrator:mybatis-mysql-table-parser

### 1.12.3. 配置参数

删除配置文件`o_g4caller_entry_method_ignore_prefix.properties`，该文件的作用可以通过其他方式替代，且该文件的存在容易造成误导，因此删除

删除配置文件`_jacg_config/config.properties`中的参数`input.ignore.other.package`
默认支持忽略，相关配置文件为空时不忽略

删除配置文件`_jacg_config/config.properties`中的参数`call.graph.jar.list`，使用新的配置文件`i_jar_dir.properties`

删除 RunnerGenAllGraph4CallerSupportIgnore 相关类，固定支持忽略特定的调用关系

数据库相关配置文件拆分，`config_db.properties`

删除配置文件`_jacg_config/config.properties`中的参数`show.method.annotation`、`gen.combined.output`、`show.caller.line.num`

在配置文件``中增加以下参数

|参数名称|参数作用|
|---|---|
|output.root.path|生成文件的根目录|
|db.insert.batch.size|批量写入数据库时每次插入的数量|
|check.jar.file.updated|检查 jar 包文件是否有更新|
|caller.show.raw.method.call.info|生成向下的方法完整调用链时，是否显示原始方法调用信息|

删除参数 PROPERTY_OUTPUT_ROOT_PATH，改到配置文件中
以上参数的说明需要删除

删除参数`write.config.in.result`

为了使`_jacg_config`目录的配置文件名更容易理解，对相关配置文件进行改名：

|修改前|修改后|
|---|---|
|i_allowed_class_prefix.properties|allowed_class_prefix.properties|
|i_jar_dir.properties|jar_dir.properties|
|o_g4callee_class_name.properties|method_class_4callee.properties|
|o_g4caller_entry_method.properties|method_class_4caller.properties|
|o_g4caller_ignore_class_keyword.properties|ignore_class_keyword.properties|
|o_g4caller_ignore_full_method_prefix.properties|ignore_full_method_prefix.properties|
|o_g4caller_ignore_method_prefix.properties|ignore_method_prefix.properties|

`ignore_class_keyword.properties`、`ignore_full_method_prefix.properties`、`ignore_method_prefix.properties`，这三个配置文件，支持在生成向上/向下的方法完整调用链时忽略指定的类或方法（以前的配置文件只支持生成向下的方法完整调用链时忽略）

假如以依赖 jar 包形式使用 java-all-call-graph，可不再执行 UnzipFile 类释放配置文件到项目中，会使用 jar 包中配置文件的默认参数值，可通过 Java 代码对配置参数进行修改

释放 java-callgraph2 配置文件`_javacg_config/config.properties`

#### 1.12.3.1. 生成当前使用的配置信息到单独的文件

#### 1.12.3.2. 不再对生成的结果文件进行合并

#### 1.12.3.3. 对配置参数进行设置的方法名称修改

为了使方法名称更能体现实际作用，将`ConfigureWrapper`类的以下方法名称进行修改：

|修改前的方法名称|修改后的方法名称|
|---|---|
|addConfig|setConfig|
|addOtherConfigSet|setOtherConfigSet|
|addOtherConfigList|setOtherConfigList|

### 1.12.4. 生成文件修改

不再生成_mapping.txt 文件（删除说明）

### 1.12.5. 数据库表

增加数据库表

长度修改

数据库表加前缀"jacg_"

### 1.12.6. 支持更准确地获取方法调用关系（支持 Spring Bean）

#### 1.12.6.1. 支持处理方法调用被调用对象及参数信息

#### 1.12.6.2. 支持增加方法调用关系

#### 1.12.6.3. 父类与子类方法调用优化

### 1.12.7. 生成向上的方法完整调用链支持忽略条件

### 1.12.8. 输出信息优化

#### 1.12.8.1. 方法循环调用

#### 1.12.8.2. 生成方法调用链文件耗时太长的辅助信息

## 1.13. (1.0.14)

配置文件`_jacg_config/config.properties`参数`call.graph.output.detail`增加`0: 最详细 完整类名+方法名+方法参数+返回类型`，可以在生成的方法完整调用链结果文件中显示方法的返回类型

配置文件`_jacg_business_data_type/business_data_type_show_4ee.properties`、`_jacg_business_data_type/business_data_type_show_4er.properties`增加可以使用的参数`method_return_generics_type`，指定以上参数时，在生成向上/向下的方法完整调用链结果文件时，显示方法返回类型中涉及集合的泛型类型

## 1.14. (1.0.15)

支持 Spring Controller 类上只有@Controller 类注解，没有@RequestMapping 类注解情况的情况

## 1.15. (2.0.0)

支持识别通过 dto 的 get/set 方法关联的字段关联关系

## 1.16. (2.0.1)

支持处理类的签名中的泛型信息

## 1.17. (2.0.2)

支持处理类的签名中继承或实现的泛型关系

## 1.18. (2.0.3)

支持将 java-callgraph2 生成的文件的数据保存到 Neo4j 图数据库

支持读取 Neo4j 保存的方法调用数据，生成向下的完整方法调用链

## 1.19. (2.0.4)

增加数据库表记录类之间的引用关系

## 1.20. (2.0.5)

增加说明文档，增加示例

## 1.21. (2.0.6)

支持将代码解析数据写入 Neo4j 时同时写入数据库中

## 1.22. (2.0.7)

同上

## 1.23. (2.0.8)

java-callgraph2 使用 2.0.8 版本

为了使 java-callgraph2 的代码及配置文件与“jacg”更容易区分，对代码及配置文件进行以下名称修改

|内容|修改前的名称|修改后的名称|
|---|---|---|
|包名|com.adrninistrator.javacg|com.adrninistrator.javacg2|
|主类|com.adrninistrator.javacg.stat.JCallGraph|com.adrninistrator.javacg2.entry.JavaCG2Entry|
|目录、文件名称|javacg|javacg2|
|类名前缀|JavaCG|JavaCG2|

`当升级到该版本时，假如之前有对目录进行过分析，则相关目录中的“-javacg_merged.jar”文件需要删除`

## 1.24. (2.0.9)

### 1.24.1. java-callgraph2 组件版本

java-callgraph2 组件升级版本为 2.0.8

### 1.24.2. 增加参数

_jacg_config/config.properties 配置文件增加参数

- drop.or.truncate.table

在插入数据库表前，对表执行 DROP(false) 还是 TRUNCATE(true) 操作

- gen.call.graph.num.limit

生成向上/向下的完整方法调用链时，每个方法允许生成的方法调用数量限制，默认为 0，小于等于 0 代表不限制

### 1.24.3. 增加数据库表

```
jacg_class_ext_impl_generics_type 类的继承或实现的泛型信息
jacg_class_signature_generics_type 类的签名中的泛型信息
jacg_javacg2_config java-callgraph2 组件使用的配置参数信息表
```

### 1.24.4. 删除数据库表

```
jacg_class_sig_ext_impl_generics 类的签名中继承或实现的泛型关系
jacg_class_signature_ei1 类的签名中涉及继承与实现的信息表 1
jacg_class_signature_generics 类的签名中的泛型信息
```

### 1.24.5. 增加字段的数据库表

主要涉及内容：是否数组、泛型相关的数据、Spring Controller 方法是否可能用于文件/上传下载

```
jacg_field_generics_type 非静态字段中涉及的泛型类型
jacg_field_info 字段信息表
jacg_get_method dto 的 get 方法及字段
jacg_method_arg_generics_type 方法参数泛型类型
jacg_method_argument 方法参数类型
jacg_method_info 方法的信息表
jacg_method_return_generics_type 方法返回泛型类型
jacg_set_method dto 的 set 方法及字段
jacg_sf_field_method_call static、final 字段初始化方法信息表
jacg_spring_controller Spring Controller 信息表
```

## 1.25. （2.1.0）

## 1.26. 增加功能

### 1.26.1. 支持查询指定枚举的指定常量名称的指定字段在初始化时的值

com.adrninistrator.jacg.handler.enums.EnumsHandler#queryEnumFieldValue

### 1.26.2. _jacg_config\config.properties 配置文件增加参数

#### 1.26.2.1. call.graph.gen.separate.stack

生成方法调用链时，是否需要为每个调用堆栈生成独立的文件，仅当 call.graph.output.detail=1 时支持

#### 1.26.2.2. parse.other.type.file

解析 jar 包时，是否对 .xml、.properties 等其他格式的文件进行解析，false: 不解析，true: 解析

### 1.26.3. 增加数据库表

```
dup_class_info  重复同名类的信息表
dup_method_info 重复同名类的方法的信息表
enum_init_arg_field 枚举类构造函数参数与字段赋值关系表
enum_init_assign_info   枚举类初始化赋值信息表
method_return_const_value   方法返回的常量值（含 null）
method_return_field_info    方法返回的字段（含枚举）
```

### 1.26.4. 修改数据库表

|表名|操作|内容|
|---|---|---|
|class_info|增加字段|class_path_in_jar 类在 jar 包中的路径|
|method_call|增加字段|callee_array_dimensions 被调用方，对象数组的维度，为 0 代表不是数组类型|
|method_call|增加字段|description 描述信息，默认为空|
|method_return_arg_seq|修改字段名|caller_method_hash 修改为 method_hash|
|method_return_arg_seq|修改字段名|caller_full_method 修改为 full_method|
|method_return_call_id|修改字段名|caller_method_hash 修改为 method_hash|
|method_return_call_id|修改字段名|caller_full_method 修改为 full_method|

### 1.26.5. 单元测试类修改

原有的 test.runbycode 包名中的类名以 TestRBC 开头的单元测试类，修改到 test.runbycodemain 包中

## 1.27. （3.0.0）

主要修改了配置参数指定方式，其他功能差别不大

### 1.27.1. 删除配置文件

- _jacg_config/jar_dir.properties

用于指定需要处理的 jar 包路径，或保存 class、jar 文件的目录路径

删除当前配置文件后，需要使用 java-callgraph2 组件的同名配置文件

- _jacg_config/allowed_class_prefix.properties

用于指定 java-callgraph2 解析类时需要处理的类名或包名前缀，以及 java-all-call-graph 在写入数据库时需要写入的类名或包名前缀

删除当前配置文件后，可以通过 java-callgraph2 组件提供的配置达到相同的控制效果

- _jacg_field_relationship/fr_eq_conversion_method.properties

当前配置文件在处理通过 get/set 方法的字段关联关系时使用，指定方法返回值与被调用对象或参数认为是等值转换的方法，在 java-callgraph2 中使用

删除当前配置文件后，需要使用 java-callgraph2 组件的同名配置文件

### 1.27.2. 增加配置文件

- _jacg_extensions/javacg2_method_call_extensions.properties

用于指定 java-callgraph2 组件在处理方法调用时的扩展类

- _jacg_extensions/jacg_method_call_extensions.properties

用于指定 java-all-call-graph 组件在处理方法调用时的扩展类

### 1.27.3. 删除数据库表

- allowed_class_prefix

允许处理的类名或包名前缀

### 1.27.4. com.adrninistrator.jacg.runner.RunnerWriteDb 类修改

使用当前类的有参数构造函数时，增加参数 1 com.adrninistrator.javacg2.conf.JavaCG2ConfigureWrapper ，需要指定 java-callgraph2 组件使用的配置包装类

修改后的构造函数参数如下：

```java
(JavaCG2ConfigureWrapper javaCG2ConfigureWrapper, ConfigureWrapper configureWrapper)
```

删除包含参数的 run 方法 run(JavaCG2ConfigureWrapper javaCG2ConfigureWrapper)

### 1.27.5. com.adrninistrator.jacg.runner.RunnerWriteCallGraphFile 类修改

同上

### 1.27.6. 支持识别通过反射调用的方法（被调用对象+被调用方法名称）

- 通过反射调用方法的工具类方法

如下所示的 TestReflectionUtil1.runByReflection() 方法用于通过反射执行被调用对象 obj 被调用方法 methodName，被调用参数使用 args

```java
public class TestReflectionUtil1 {
    public static void runByReflection(Object obj, String methodName, Object... args) {
        ...
```

例如以下代码，代表通过反射执行 FRCDtoA 类的 testStrFRCDtoA 方法，使用的参数为"test2DMethodCallReturn"

```java
  TestReflectionUtil1.runByReflection(new FRCDtoA(), "testStrFRCDtoA", "test2DMethodCallReturn");
```

- 支持识别以上方法调用的示例代码

在示例代码 test.runbycode.extensions.methodcall.TestAddMethodCall4Reflection1 中展示了怎样识别以上通过反射调用的方法，并补充对应的调用关系

需要在 java-all-call-graph 组件的配置文件 _jacg_extensions/javacg2_method_call_extensions.properties 中指定扩展类，并在配置文件 _jacg_extensions/jacg_method_call_extensions.properties 中指定扩展类，在调用 com.adrninistrator.jacg.runner.RunnerWriteDb 类解析 jar 文件并写入数据库前指定

参考对应扩展类：test.runbycode.extensions.methodcall.JavaCG2Reflection1MethodCallExtension、test.runbycode.extensions.methodcall.JACGReflection1MethodCallExtension

## 1.28. （3.0.1）

### 1.28.1. 支持识别通过反射调用的方法（被调用对象+被调用方法名称+被调用方法参数类型）

支持区分被调用方法参数类型，示例类同上

## 1.29. (3.0.2)

### 1.29.1. 支持为通过 Apache Commons Chain 调用的方法补充方法调用

- 通过 Apache Commons Chain 调用方法的示例

支持为 org.apache.commons.chain.impl.ChainBase:addCommand(org.apache.commons.chain.Command) 方法补充被调用方法

例如在测试类 test.callgraph.chain.use.TestUseChain1 中，通过以下方式调用了以上方法：

```java
    @Resource(name = TestChainCommandService1.SERVICE_NAME)
    private Command commandService1;

    @Autowired
    @Qualifier(TestChainCommandService2.SERVICE_NAME)
    private Command commandService2;

    @Test
    public void run() throws Exception {
        ChainBase chain = new ChainBase();
        chain.addCommand(new TestChainCommand1());
        chain.addCommand(commandService1);
        chain.addCommand(commandService2);
        Context context = new ContextBase();
        chain.execute(context);
    }
```

- 支持识别以上方法调用的实现方式

增加一条方法调用，调用者方法为 org.apache.commons.chain.impl.ChainBase:addCommand(org.apache.commons.chain.Command) ，被调用者方法为以上方法参数 1 对应的参数类型，即 org.apache.commons.chain.Command 接口的实现类类型

- 支持识别以上方法调用的示例代码

在示例代码 test.runbycode.extensions.methodcall.apachecommonschain.TestAddMethodCall4ApacheCommonsChain 中展示了怎样识别以上通过 Apache Commons Chain 调用的方法，并补充对应的调用关系

需要在 java-all-call-graph 组件的配置文件 _jacg_extensions/javacg2_method_call_extensions.properties 中指定扩展类 com.adrninistrator.jacg.extensions.methodcall.JavaCG2ApacheCommonsChainMethodCallExtension ，并在配置文件 _jacg_extensions/jacg_method_call_extensions.properties 中指定扩展类 com.adrninistrator.jacg.extensions.methodcall.JACGApacheCommonsChainMethodCallExtension ，在调用 com.adrninistrator.jacg.runner.RunnerWriteDb 类解析 jar 文件并写入数据库前指定

- 忽略 org.apache.commons.chain.Command:execute() 方法调用实现类方法的调用

假如在解析 jar 文件时指定了 Apache Commons Chain 的 commons-chain-xx.jar，则需要通过 java-callgraph2 组件的表达式配置文件 _javacg2_parse_method_call_switch/parse_ignore_method_call_er.av 指定忽略 org.apache.commons.chain.Command:execute() 方法调用实现类方法的调用，以避免以上方法调用出现在生成的完整方法调用链中

### 1.29.2. 支持识别方法调用中被调用对象与参数使用的值（支持枚举）

支持识别方法调用中被调用对象与参数使用的值，包括指定常量、枚举常量、枚举常量的方法调用返回值

增加方法 com.adrninistrator.jacg.handler.methodcall.MethodCallInfoHandler:queryMethodCallWithValueSupportEnum(java.lang.String, java.lang.String, int...) 用于实现以上功能

示例代码参考 test.runbycode.handler.methodcallargs.TestMethodCallArgValue

### 1.29.3. 完善 gradle 任务 gen_run_jar

复制 java-callgraph2 组件中的配置文件

复制 test 模块依赖组件的 jar 文件更准确

## 1.30. (3.0.3)

### 1.30.1. 删除配置参数

#### 1.30.1.1. _jacg_config/config.properties

```
# 解析 jar 包时，是否对。xml、.properties 等其他格式的文件进行解析，false: 不解析，true: 解析
parse.other.type.file=true

# 解析 jar 包时，是否处理通过 get/set 方法关联的字段关联关系，false: 不处理，true: 处理
handle.get.set.field.relationship=false
```

java-callgraph2 相关的配置参数改为直接在 java-callgraph2 中指定

### 1.30.2. 增加配置参数

```
# 生成调用链时，是否需要将调用链数据写入文件
call.graph.write.to.file=true

# 生成调用链时，是否需要在内存中返回调用链数据
# 不能与 call.graph.write.to.file 开关同时设置为 false
# _jacg_config/method_class_4caller.properties 或 _jacg_config/method_class_4callee.properties 配置文件中只能一个需要处理的方法
# 设置为 true 时，可通过 gen.call.graph.num.limit 开关设置允许生成的方法调用数量限制
call.graph.return.in.memory=false

# 生成调用链时，文件名是否使用更短的模式，以避免超过 Windows 文件系统支持的长度
# 若是，则文件名仅包含对应方法的 HASH+长度；若否，则文件名还会包含方法的唯一类名及方法名
call.graph.file.short.mode=false
```

### 1.30.3. 增加功能

- 生成调用链时，支持在内存中返回调用链数据

参考以下方法

```
test.runbycodemain.TestRBCRunnerGenAllGraph4Callee#testReturnInMemory
test.runbycodemain.TestRBCRunnerGenAllGraph4Caller#testReturnInMemory
```

配置参数如上

- 生成调用链时，文件名支持使用更短的模式

生成的调用链文件名为 {方法 HASH+长度}.txt

配置参数如上

### 1.30.4. 生成调用链优化

生成方法完整调用链时，假如指定的类或方法不存在，也不会报错

当生成的方法调用链为空（向下或向上没有被调用方法或调用方法）时，调用链文件（排除文件后缀）以“!empty”结尾

当指定的类或方法不存在时，调用链文件（排除文件后缀）以“!not_found”结尾

### 1.30.5. 增加数据库表

#### 1.30.5.1. jacg_mybatis_ms_formated_sql

MyBatis XML 中格式化后的 sql 文本（使用 MySQL）

## 1.31. (3.0.4)

修复 bug

```log
ERROR AbstractWriteDbHandler.handle(500) - 出现异常 WriteDbHandler4ClassReference 
com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException: 当前类依赖的数据库表还未写入 WriteDbHandler4ClassName DTIE_CLASS_REFERENCE
```

## 1.32. (3.0.5)

优化 _jacg_config/config.properties 配置文件参数 call.graph.write.to.file=false，call.graph.return.in.memory=true 时返回的内存中的方法调用链不包含方法注解的问题
