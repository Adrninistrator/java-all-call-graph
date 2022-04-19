# 1. java-callgraph2提供的扩展功能

## 1.1. 处理方法上的注解信息的插件功能

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

# 2. java-all-call-graph提供的扩展功能