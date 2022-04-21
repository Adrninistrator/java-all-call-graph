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
|增加|resources/~jacg_extensions/method_annotation_handler.properties|定义用于对方法上的注解进行处理的类完整类名|
|增加|resources/~jacg_sql/class_annotation.sql|用于保存类上的注解信息数据库表|
|修改|resources/~jacg_sql/method_annotation.sql|增加了保存注解属性的字段|

### 1.2.2. 提供处理方法上的注解信息的插件功能

在新增的method_annotation_handler.properties配置文件中，可以定义用于对方法上的注解进行处理的类完整类名，该文件每行指定一项配置，可指定多行

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

新增了以下类，可通过其方法对参数配置进行设置

```java
com.adrninistrator.jacg.conf.ConfigureWrapper
```

在执行释放到项目中的test.jacg包中的入口类（如TestRunnerWriteDb），或执行jar包中com.adrninistrator.jacg.runner包中的入口类（如RunnerWriteDb）之前，需要先调用ConfigureWrapper类的方法设置参数配置

以下可参考`test.run_by_code`包中的测试代码，在`TestRunByCodeBase`类中调用了ConfigureWrapper类的方法

### 1.3.1. 设置~jacg_config/config.properties配置文件参数

```java
ConfigureWrapper.addConfig(ConfigKeyEnum configKeyEnum, String value);
```

`对于app.name参数，在以上方法中会将参数值中的-替换为_`

ConfigKeyEnum枚举类中定义了~jacg_config/config.properties配置文件中的参数key

通过value参数指定需要设置的参数值

示例如下：

```java
ConfigureWrapper.addConfig(ConfigKeyEnum.CKE_APPNAME, "test_rbc");
```

### 1.3.2. 设置~jacg_config目录其他配置文件参数

```java
ConfigureWrapper.addOtherConfigSet(OtherConfigFileUseSetEnum otherConfigFileUseSetEnum, Set<String> configSet);
```

OtherConfigFileUseSetEnum枚举类中定义了~jacg_config目录中其他配置文件的文件名

通过configSet参数指定需要设置的Set类型的参数值

示例如下：

```java
ConfigureWrapper.addOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_IN_ALLOWED_CLASS_PREFIX, new HashSet(Arrays.asList(
        "test.call_graph.method_call", "test.call_graph.argument", "java.")));
```

### 1.3.3. 设置~jacg_find_keyword目录配置文件参数

```java
ConfigureWrapper.addOtherConfigList(OtherConfigFileUseListEnum otherConfigFileUseListEnum, List<String> configList);
```

OtherConfigFileUseListEnum枚举类中定义了~jacg_find_keyword目录中配置文件的文件名

通过configList参数指定需要设置的List类型的参数值

示例如下：

```java
ConfigureWrapper.addOtherConfigList(OtherConfigFileUseListEnum.OCFULE_FIND_KEYWORD_4CALLEE, Arrays.asList("!entry!", "<init>"));
```
