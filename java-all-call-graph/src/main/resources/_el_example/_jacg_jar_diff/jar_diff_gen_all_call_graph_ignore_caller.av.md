# 1. 表达式配置文件说明

- 表达式配置文件名称

_jacg_jar_diff/jar_diff_gen_all_call_graph_ignore_caller.av

- 表达式配置文件作用

在 对JarDiff找到发生变化的方法生成向上/向下方法完整调用链时 使用的配置参数

表达式配置参数的说明如下：

若当前配置文件中的表达式执行结果为 true，则跳过为对应的方法生成方法完整调用链

若表达式执行结果为 false，或未指定表达式，则当前配置不会跳过对应的方法

JarDiff获得发生变化的方法向下的方法完整调用链时，指定发生变化的方法中，需要忽略的方法

- 表达式配置文件对应的枚举常量名

ElConfigEnum.ECE_JAR_DIFF_GEN_ALL_CALL_GRAPH_IGNORE_CALLER

以下为支持的表达式变量示例

# 2. class_name - JarDiff生成向下方法完整调用链判断发生变化的方法类名

- 表达式变量说明

完整类名

- 表达式示例说明

通过JarDiff生成向下方法完整调用链时，判断发生变化的方法类名是否等于指定关键字，忽略匹配的方法

- 表达式示例文本

```js
class_name == 'test.diffjar.controller.TestController1'
```

- 表达式示例文本 - 代码中指定

```java
JavaCG2ElAllowedVariableEnum.EAVE_PARSE_CLASS_NAME.getVariableName() + " == '" + test.diffjar.controller.TestController1.class.getName() + "'"
```

- 表达式示例类名

test.runbycode.el.jardiff.TestGenJarDiffCallerGraph4ClassName

# 3. package_name - JarDiff生成向下方法完整调用链判断发生变化的方法包名

- 表达式变量说明

完整包名

不会以.结束

- 表达式示例说明

通过JarDiff生成向下方法完整调用链时，判断发生变化的方法包名是否等于指定关键字，忽略匹配的方法

- 表达式示例文本

```js
package_name == 'test.diffjar.controller'
```

- 表达式示例文本 - 代码中指定

```java
JavaCG2ElAllowedVariableEnum.EAVE_PARSE_PACKAGE_NAME.getVariableName() + " == 'test.diffjar.controller'"
```

- 表达式示例类名

test.runbycode.el.jardiff.TestGenJarDiffCallerGraph4PackageName

# 4. simple_class_name - JarDiff生成向下方法完整调用链判断发生变化的方法简单类名

- 表达式变量说明

简单类名

- 表达式示例说明

通过JarDiff生成向下方法完整调用链时，判断发生变化的方法简单类名是否等于指定关键字，忽略匹配的方法

- 表达式示例文本

```js
simple_class_name == 'TestController1'
```

- 表达式示例文本 - 代码中指定

```java
JavaCG2ElAllowedVariableEnum.EAVE_PARSE_SIMPLE_CLASS_NAME.getVariableName() + " == '" + test.diffjar.controller.TestController1.class.getSimpleName() + "'"
```

- 表达式示例类名

test.runbycode.el.jardiff.TestGenJarDiffCallerGraph4SimpleClassName

# 5. method_name - JarDiff生成向下方法完整调用链判断发生变化的方法名

- 表达式变量说明

方法名

不包括括号及方法参数

- 表达式示例说明

通过JarDiff生成向下方法完整调用链时，判断发生变化的方法名是否等于指定关键字，忽略匹配的方法

- 表达式示例文本

```js
method_name == 'get1'
```

- 表达式示例文本 - 代码中指定

```java
JavaCG2ElAllowedVariableEnum.EAVE_PARSE_METHOD_NAME.getVariableName() + " == 'get1'"
```

- 表达式示例类名

test.runbycode.el.jardiff.TestGenJarDiffCallerGraph4MethodName

# 6. method_arg_num - JarDiff生成向下方法完整调用链判断发生变化的方法参数数量

- 表达式变量说明

方法参数数量

- 表达式示例说明

通过JarDiff生成向下方法完整调用链时，判断发生变化的方法参数数量是否等于指定值，忽略匹配的方法

- 表达式示例文本

```js
method_arg_num == 0
```

- 表达式示例文本 - 代码中指定

```java
JavaCG2ElAllowedVariableEnum.EAVE_PARSE_METHOD_ARG_NUM.getVariableName() + " == 0"
```

- 表达式示例类名

test.runbycode.el.jardiff.TestGenJarDiffCallerGraph4MethodArgNum

# 7. full_method - JarDiff生成向下方法完整调用链判断发生变化的完整方法

- 表达式变量说明

完整方法

- 表达式示例说明

通过JarDiff生成向下方法完整调用链时，判断发生变化的完整方法是否等于指定关键字，忽略匹配的方法

- 表达式示例文本

```js
full_method == 'test.diffjar.controller.TestController1:get1()'
```

- 表达式示例文本 - 代码中指定

```java
JavaCG2ElAllowedVariableEnum.EAVE_PARSE_FULL_METHOD.getVariableName() + " == '" + test.diffjar.controller.TestController1.class.getName() + ":get1()'"
```

- 表达式示例类名

test.runbycode.el.jardiff.TestGenJarDiffCallerGraph4FullMethod

