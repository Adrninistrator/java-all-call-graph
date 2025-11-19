# 1. 表达式配置文件说明

- 表达式配置文件名称

_jacg_spring_aop/spring_aop_ignore_spring_bean_class.av

- 表达式配置文件作用

在 解析Spring AOP影响方法时 使用的配置参数

表达式配置参数的说明如下：

若当前配置文件中的表达式执行结果为 true，则跳过处理对应的Spring Bean

若表达式执行结果为 false，或未指定表达式，则当前配置不会跳过对应的Spring Bean

指定解析Spring AOP影响方法时忽略哪些Spring Bean类，支持指定类名、包名、简单类名

- 表达式配置文件对应的枚举常量名

ElConfigEnum.ECE_SPRING_AOP_IGNORE_SPRING_BEAN_CLASS

以下为支持的表达式变量示例

# 2. class_name - 解析Spring AOP影响方法判断受影响Bean类名

- 表达式变量说明

完整类名

- 表达式示例说明

在解析Spring AOP影响方法时，判断受影响Bean类名是否等于指定关键字，忽略匹配的Bean

- 表达式示例文本

```js
class_name == 'test.callgraph.spring.aop.annopointcut1.service.TestSpringAOPAnnoPointcutUserServiceImpl1'
```

- 表达式示例文本 - 代码中指定

```java
JavaCG2ElAllowedVariableEnum.EAVE_PARSE_CLASS_NAME.getVariableName() + " == '" + test.callgraph.spring.aop.annopointcut1.service.TestSpringAOPAnnoPointcutUserServiceImpl1.class.getName() + "'"
```

- 代码中指定表达式示例

```java
configureWrapper.setElConfigText(ElConfigEnum.ECE_SPRING_AOP_IGNORE_SPRING_BEAN_CLASS, 
    JavaCG2ElAllowedVariableEnum.EAVE_PARSE_CLASS_NAME.getVariableName() + " == '" + test.callgraph.spring.aop.annopointcut1.service.TestSpringAOPAnnoPointcutUserServiceImpl1.class.getName() + "'"
);
```

- 表达式示例类名

test.runbycode.el.springaop.TestSpringAopEl4SpringBeanClassName

# 3. package_name - 解析Spring AOP影响方法判断受影响Bean包名

- 表达式变量说明

完整包名

不会以.结束

- 表达式示例说明

在解析Spring AOP影响方法时，判断受影响Bean包名是否等于指定关键字，忽略匹配的Bean

- 表达式示例文本

```js
package_name == 'test.callgraph.spring.aop.annopointcut1.service'
```

- 表达式示例文本 - 代码中指定

```java
JavaCG2ElAllowedVariableEnum.EAVE_PARSE_PACKAGE_NAME.getVariableName() + " == 'test.callgraph.spring.aop.annopointcut1.service'"
```

- 代码中指定表达式示例

```java
configureWrapper.setElConfigText(ElConfigEnum.ECE_SPRING_AOP_IGNORE_SPRING_BEAN_CLASS, 
    JavaCG2ElAllowedVariableEnum.EAVE_PARSE_PACKAGE_NAME.getVariableName() + " == 'test.callgraph.spring.aop.annopointcut1.service'"
);
```

- 表达式示例类名

test.runbycode.el.springaop.TestSpringAopEl4SpringBeanPackageName

# 4. simple_class_name - 解析Spring AOP影响方法判断受影响Bean简单类名

- 表达式变量说明

简单类名

- 表达式示例说明

在解析Spring AOP影响方法时，判断受影响Bean简单类名是否等于指定关键字，忽略匹配的Bean

- 表达式示例文本

```js
simple_class_name == 'TestSpringAOPAnnoPointcutUserServiceImpl1'
```

- 表达式示例文本 - 代码中指定

```java
JavaCG2ElAllowedVariableEnum.EAVE_PARSE_SIMPLE_CLASS_NAME.getVariableName() + " == '" + test.callgraph.spring.aop.annopointcut1.service.TestSpringAOPAnnoPointcutUserServiceImpl1.class.getSimpleName() + "'"
```

- 代码中指定表达式示例

```java
configureWrapper.setElConfigText(ElConfigEnum.ECE_SPRING_AOP_IGNORE_SPRING_BEAN_CLASS, 
    JavaCG2ElAllowedVariableEnum.EAVE_PARSE_SIMPLE_CLASS_NAME.getVariableName() + " == '" + test.callgraph.spring.aop.annopointcut1.service.TestSpringAOPAnnoPointcutUserServiceImpl1.class.getSimpleName() + "'"
);
```

- 表达式示例类名

test.runbycode.el.springaop.TestSpringAopEl4SpringBeanSimpleClassName

