# 1. 表达式配置文件说明

- 表达式配置文件名称

_jacg_gen_all_call_graph/gen_call_graph_ignore_method_call.av

- 表达式配置文件作用

在 生成方法完整调用链时 使用的配置参数

表达式配置参数的说明如下：

若当前配置文件中的表达式执行结果为 true，则跳过在方法完整调用链中生成对应的方法

若表达式执行结果为 false，或未指定表达式，则当前配置不会跳过对应的方法

指定生成方法完整调用链时是否跳过解析特定的方法调用，支持通过方法调用类型、调用方法或被调用方法等判断

- 表达式配置文件对应的枚举常量名

ElConfigEnum.ECE_GEN_ALL_CALL_GRAPH_IGNORE_METHOD_CALL

以下为支持的表达式变量示例

# 2. method_call_type - 生成向上方法完整调用链判断调用类型

- 表达式变量说明

方法调用类型

参考 JavaCG2CallTypeEnum 类

- 表达式示例说明

在生成向上方法完整调用链时，判断调用类型是否等于指定值，忽略匹配的方法调用

- 表达式示例文本

```js
method_call_type == 'STA'
```

- 表达式示例文本 - 代码中指定

```java
CommonElAllowedVariableEnum.EAVE_METHOD_CALL_TYPE.getVariableName() + " == '" + JavaCG2CallTypeEnum.CTE_RAW_INVOKE_STATIC.getType() + "'"
```

- 表达式示例类名

test.runbycode.el.gencallgraph.TestGenCallGraph4EeElMethodCallType

# 3. er_class_name - 生成向下方法完整调用链判断调用类名

- 表达式变量说明

调用方完整类名

- 表达式示例说明

在生成向下方法完整调用链时，判断调用类名是否等于指定关键字，忽略匹配的方法调用

- 表达式示例文本

```js
er_class_name == 'test.callgraph.elexample.caller.TestElExampleCaller1'
```

- 表达式示例文本 - 代码中指定

```java
CommonElAllowedVariableEnum.EAVE_MC_ER_CLASS_NAME.getVariableName() + " == '" + test.callgraph.elexample.caller.TestElExampleCaller1.class.getName() + "'"
```

- 表达式示例类名

test.runbycode.el.gencallgraph.TestGenCallGraph4ErElErClassName

# 4. er_package_name - 生成向上方法完整调用链判断调用类包名

- 表达式变量说明

调用方完整包名

不会以.结束

- 表达式示例说明

在生成向上方法完整调用链时，判断调用类包名是否等于指定值，忽略匹配的方法调用

- 表达式示例文本

```js
er_package_name == 'test.callgraph.elexample.caller'
```

- 表达式示例文本 - 代码中指定

```java
CommonElAllowedVariableEnum.EAVE_MC_ER_PACKAGE_NAME.getVariableName() + " == '" + "test.callgraph.elexample.caller" + "'"
```

- 表达式示例类名

test.runbycode.el.gencallgraph.TestGenCallGraph4EeElErPackageName

# 5. er_simple_class_name - 生成向上方法完整调用链判断调用类简单类名

- 表达式变量说明

调用方简单类名

- 表达式示例说明

在生成向上方法完整调用链时，判断调用类简单类名是否等于指定值，忽略匹配的方法调用

- 表达式示例文本

```js
er_simple_class_name == 'TestElExampleCaller1'
```

- 表达式示例文本 - 代码中指定

```java
CommonElAllowedVariableEnum.EAVE_MC_ER_SIMPLE_CLASS_NAME.getVariableName() + " == '" + test.callgraph.elexample.caller.TestElExampleCaller1.class.getSimpleName() + "'"
```

- 表达式示例类名

test.runbycode.el.gencallgraph.TestGenCallGraph4EeElErSimpleClassName

# 6. er_method_name - 生成向上方法完整调用链判断调用方法名

- 表达式变量说明

调用方方法名

不包括括号及方法参数

- 表达式示例说明

在生成向上方法完整调用链时，判断调用方法名是否等于指定值，忽略匹配的方法调用

- 表达式示例文本

```js
er_method_name == 'test1'
```

- 表达式示例文本 - 代码中指定

```java
CommonElAllowedVariableEnum.EAVE_MC_ER_METHOD_NAME.getVariableName() + " == 'test1'"
```

- 表达式示例类名

test.runbycode.el.gencallgraph.TestGenCallGraph4EeElErMethodName

# 7. er_method_arg_num - 生成向上方法完整调用链判断调用方法参数数量

- 表达式变量说明

调用方方法参数数量

- 表达式示例说明

在生成向上方法完整调用链时，判断调用方法参数数量是否等于指定值，忽略匹配的方法调用

- 表达式示例文本

```js
er_method_arg_num == 1
```

- 表达式示例文本 - 代码中指定

```java
CommonElAllowedVariableEnum.EAVE_MC_ER_METHOD_ARG_NUM.getVariableName() + " == 1"
```

- 表达式示例类名

test.runbycode.el.gencallgraph.TestGenCallGraph4EeElErMethodArgNum

# 8. er_full_method - 生成向下方法完整调用链判断调用完整方法

- 表达式变量说明

调用方完整方法

包括括号及方法参数

- 表达式示例说明

在生成向下方法完整调用链时，判断调用完整方法是否等于指定关键字，忽略匹配的方法调用

- 表达式示例文本

```js
er_full_method == 'test.callgraph.elexample.caller.TestElExampleCaller1:test1(java.lang.String)'
```

- 表达式示例文本 - 代码中指定

```java
CommonElAllowedVariableEnum.EAVE_MC_ER_FULL_METHOD.getVariableName() + " == '" + test.callgraph.elexample.caller.TestElExampleCaller1.class.getName() + ":test1(" + java.lang.String.class.getName() + ")'"
```

- 表达式示例类名

test.runbycode.el.gencallgraph.TestGenCallGraph4ErElErFullMethod

# 9. ee_class_name - 生成向上方法完整调用链判断被调用类名

- 表达式变量说明

被调用方完整类名

- 表达式示例说明

在生成向上方法完整调用链时，判断被调用类名是否等于指定关键字，忽略匹配的方法调用

- 表达式示例文本

```js
ee_class_name == 'test.callgraph.elexample.callee.TestElExampleCallee1'
```

- 表达式示例文本 - 代码中指定

```java
CommonElAllowedVariableEnum.EAVE_MC_EE_CLASS_NAME.getVariableName() + " == '" + test.callgraph.elexample.callee.TestElExampleCallee1.class.getName() + "'"
```

- 表达式示例类名

test.runbycode.el.gencallgraph.TestGenCallGraph4EeElEeClassName

# 10. ee_package_name - 生成向下方法完整调用链判断被调用类包名

- 表达式变量说明

被调用方完整包名

不会以.结束

- 表达式示例说明

在生成向下方法完整调用链时，判断被调用类包名是否等于指定值，忽略匹配的方法调用

- 表达式示例文本

```js
ee_package_name == 'test.callgraph.elexample.callee'
```

- 表达式示例文本 - 代码中指定

```java
CommonElAllowedVariableEnum.EAVE_MC_EE_PACKAGE_NAME.getVariableName() + " == '" + "test.callgraph.elexample.callee" + "'"
```

- 表达式示例类名

test.runbycode.el.gencallgraph.TestGenCallGraph4ErElEePackageName

# 11. ee_simple_class_name - 生成向下方法完整调用链判断被调用类简单类名

- 表达式变量说明

被调用方简单类名

- 表达式示例说明

在生成向下方法完整调用链时，判断被调用类简单类名是否等于指定值，忽略匹配的方法调用

- 表达式示例文本

```js
ee_simple_class_name == 'TestElExampleCallee1'
```

- 表达式示例文本 - 代码中指定

```java
CommonElAllowedVariableEnum.EAVE_MC_EE_SIMPLE_CLASS_NAME.getVariableName() + " == '" + test.callgraph.elexample.callee.TestElExampleCallee1.class.getSimpleName() + "'"
```

- 表达式示例类名

test.runbycode.el.gencallgraph.TestGenCallGraph4ErElEeSimpleClassName

# 12. ee_method_name - 生成向下方法完整调用链判断被调用方法名

- 表达式变量说明

被调用方方法名

不包括括号及方法参数

- 表达式示例说明

在生成向下方法完整调用链时，判断被调用方法名是否等于指定值，忽略匹配的方法调用

- 表达式示例文本

```js
ee_method_name == 'testA'
```

- 表达式示例文本 - 代码中指定

```java
CommonElAllowedVariableEnum.EAVE_MC_EE_METHOD_NAME.getVariableName() + " == 'testA'"
```

- 表达式示例类名

test.runbycode.el.gencallgraph.TestGenCallGraph4ErElEeMethodName

# 13. ee_method_arg_num - 生成向下方法完整调用链判断被调用方法参数数量

- 表达式变量说明

被调用方方法参数数量

- 表达式示例说明

在生成向下方法完整调用链时，判断被调用方法参数数量是否等于指定值，忽略匹配的方法调用

- 表达式示例文本

```js
ee_method_arg_num == 0
```

- 表达式示例文本 - 代码中指定

```java
CommonElAllowedVariableEnum.EAVE_MC_EE_METHOD_ARG_NUM.getVariableName() + " == 0"
```

- 表达式示例类名

test.runbycode.el.gencallgraph.TestGenCallGraph4ErElEeMethodArgNum

# 14. ee_full_method - 生成向上方法完整调用链判断被调用完整方法

- 表达式变量说明

被调用方完整方法

包括括号及方法参数

- 表达式示例说明

在生成向上方法完整调用链时，判断被调用完整方法是否等于指定关键字，忽略匹配的方法调用

- 表达式示例文本

```js
ee_full_method == 'test.callgraph.elexample.callee.TestElExampleCallee1:testC()'
```

- 表达式示例文本 - 代码中指定

```java
CommonElAllowedVariableEnum.EAVE_MC_EE_FULL_METHOD.getVariableName() + " == '" + test.callgraph.elexample.callee.TestElExampleCallee1.class.getName() + ":testC()'"
```

- 表达式示例类名

test.runbycode.el.gencallgraph.TestGenCallGraph4EeElEeFullMethod

# 15. flags_enum - 生成向上方法完整调用链忽略所有调用标志

- 表达式变量说明

方法调用标志枚举

指定 MethodCallFlagsEnum 类的常量名称，代表需要处理包含/不包含哪些标志的方法调用

如 MCFE_ER_METHOD_ANNOTATION

- 表达式示例说明

在生成向上方法完整调用链时，忽略所有存在调用标志的方法调用

- 表达式示例文本

```js
count(flags_enum) > 0
```

- 表达式示例文本 - 代码中指定

```java
"count(" + ElAllowedVariableEnum.EAVE_MC_FLAGS_ENUM.getVariableName() + ") > 0"
```

- 表达式示例类名

test.runbycode.el.gencallgraph.TestGenCallGraph4EeElFlagsExcludeAll

# 16. flags_enum - 生成向下方法完整调用链判断调用标志多个条件使用与

- 表达式变量说明

方法调用标志枚举

指定 MethodCallFlagsEnum 类的常量名称，代表需要处理包含/不包含哪些标志的方法调用

如 MCFE_ER_METHOD_ANNOTATION

- 表达式示例说明

在生成向下方法完整调用链时，判断调用标志是否包括指定的多个标志中的每一个，忽略匹配的方法调用

- 表达式示例文本

```js
include(flags_enum, 'MCFE_EE_METHOD_ANNOTATION') && include(flags_enum, 'MCFE_METHOD_CALL_INFO')
```

- 表达式示例文本 - 代码中指定

```java
"include(" + ElAllowedVariableEnum.EAVE_MC_FLAGS_ENUM.getVariableName() + ", '" + MethodCallFlagsEnum.MCFE_EE_METHOD_ANNOTATION.name() + "') && include(" + ElAllowedVariableEnum.EAVE_MC_FLAGS_ENUM.getVariableName() + ", '" + MethodCallFlagsEnum.MCFE_METHOD_CALL_INFO.name() + "')"
```

- 表达式示例类名

test.runbycode.el.gencallgraph.TestGenCallGraph4ErElFlagsExcludeMultiAnd

# 17. flags_enum - 生成向下方法完整调用链判断调用标志多个条件使用或

- 表达式变量说明

方法调用标志枚举

指定 MethodCallFlagsEnum 类的常量名称，代表需要处理包含/不包含哪些标志的方法调用

如 MCFE_ER_METHOD_ANNOTATION

- 表达式示例说明

在生成向下方法完整调用链时，判断调用标志是否包括指定的多个标志中的任意一个，忽略匹配的方法调用

- 表达式示例文本

```js
include(flags_enum, 'MCFE_ER_METHOD_ANNOTATION') || include(flags_enum, 'MCFE_METHOD_CALL_INFO')
```

- 表达式示例文本 - 代码中指定

```java
"include(" + ElAllowedVariableEnum.EAVE_MC_FLAGS_ENUM.getVariableName() + ", '" + MethodCallFlagsEnum.MCFE_ER_METHOD_ANNOTATION.name() + "') || include(" + ElAllowedVariableEnum.EAVE_MC_FLAGS_ENUM.getVariableName() + ", '" + MethodCallFlagsEnum.MCFE_METHOD_CALL_INFO.name() + "')"
```

- 表达式示例类名

test.runbycode.el.gencallgraph.TestGenCallGraph4ErElFlagsExcludeMultiOr

# 18. flags_enum - 生成向下方法完整调用链判断调用标志

- 表达式变量说明

方法调用标志枚举

指定 MethodCallFlagsEnum 类的常量名称，代表需要处理包含/不包含哪些标志的方法调用

如 MCFE_ER_METHOD_ANNOTATION

- 表达式示例说明

在生成向下方法完整调用链时，判断调用标志是否包括指定的标志，忽略匹配的方法调用

- 表达式示例文本

```js
include(flags_enum, 'MCFE_EE_DTO_GET_SET_METHOD')
```

- 表达式示例文本 - 代码中指定

```java
"include(" + ElAllowedVariableEnum.EAVE_MC_FLAGS_ENUM.getVariableName() + ", '" + MethodCallFlagsEnum.MCFE_EE_DTO_GET_SET_METHOD.name() + "')"
```

- 表达式示例类名

test.runbycode.el.gencallgraph.TestGenCallGraph4ErElFlagsExcludeOne

