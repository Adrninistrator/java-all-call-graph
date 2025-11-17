# 1. 表达式配置文件说明

当前文件的内容为表达式中的字符串比较示例

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

# 2. er_class_name - 判断多个条件使用与

- 表达式变量说明

调用方完整类名

- 表达式示例说明

在生成向上方法完整调用链时，多个条件使用与运算，判断调用类名是否以指定关键字开头，且以指定关键字结尾，忽略匹配的类

- 表达式示例文本

```js
string.startsWith(er_class_name, 'test.callgraph.elexample.caller.') && string.endsWith(er_class_name, 'TestElExampleCaller1')
```

- 表达式示例文本 - 代码中指定

```java
"string.startsWith(" + CommonElAllowedVariableEnum.EAVE_MC_ER_CLASS_NAME.getVariableName() + ", 'test.callgraph.elexample.caller.') && string.endsWith(" + CommonElAllowedVariableEnum.EAVE_MC_ER_CLASS_NAME.getVariableName() + ", '" + test.callgraph.elexample.caller.TestElExampleCaller1.class.getSimpleName() + "')"
```

- 表达式示例类名

test.runbycode.el.stringcompare.TestGenCallGraph4EeElErClassNameAnd

# 3. er_class_name - 判断包含关键字

- 表达式变量说明

调用方完整类名

- 表达式示例说明

在生成向上方法完整调用链时，判断调用类名是否包含指定关键字，忽略匹配的方法调用

- 表达式示例文本

```js
string.contains(er_class_name, 'test.callgraph.elexample.caller.TestElExampleCaller1')
```

- 表达式示例文本 - 代码中指定

```java
"string.contains(" + CommonElAllowedVariableEnum.EAVE_MC_ER_CLASS_NAME.getVariableName() + ", '" + test.callgraph.elexample.caller.TestElExampleCaller1.class.getName() + "')"
```

- 表达式示例类名

test.runbycode.el.stringcompare.TestGenCallGraph4EeElErClassNameContains

# 4. er_class_name - 判断包含多个关键字之一

- 表达式变量说明

调用方完整类名

- 表达式示例说明

在生成向上方法完整调用链时，判断调用类名是否包含指定多个关键字之一，忽略匹配的方法调用

- 表达式示例文本

```js
string.containsAny(er_class_name, 'test.callgraph.elexample.caller.TestElExampleCaller1', 'test.callgraph.elexample.callee.TestElExampleCallee1')
```

- 表达式示例文本 - 代码中指定

```java
"string.containsAny(" + CommonElAllowedVariableEnum.EAVE_MC_ER_CLASS_NAME.getVariableName() + ", '" + test.callgraph.elexample.caller.TestElExampleCaller1.class.getName() + "', '" + test.callgraph.elexample.callee.TestElExampleCallee1.class.getName() + "')"
```

- 表达式示例类名

test.runbycode.el.stringcompare.TestGenCallGraph4EeElErClassNameContainsAny

# 5. er_class_name - 判断以关键字结尾

- 表达式变量说明

调用方完整类名

- 表达式示例说明

在生成向上方法完整调用链时，判断调用类名是否以指定关键字结尾，忽略匹配的方法调用

- 表达式示例文本

```js
string.endsWith(er_class_name, 'test.callgraph.elexample.caller.TestElExampleCaller1')
```

- 表达式示例文本 - 代码中指定

```java
"string.endsWith(" + CommonElAllowedVariableEnum.EAVE_MC_ER_CLASS_NAME.getVariableName() + ", '" + test.callgraph.elexample.caller.TestElExampleCaller1.class.getName() + "')"
```

- 表达式示例类名

test.runbycode.el.stringcompare.TestGenCallGraph4EeElErClassNameEndsWith

# 6. er_class_name - 判断以多个关键字之一结尾

- 表达式变量说明

调用方完整类名

- 表达式示例说明

在生成向上方法完整调用链时，判断调用类名是否以指定多个关键字之一结尾，忽略匹配的方法调用

- 表达式示例文本

```js
string.endsWithAny(er_class_name, 'test.callgraph.elexample.caller.TestElExampleCaller1', 'test.callgraph.elexample.callee.TestElExampleCallee1')
```

- 表达式示例文本 - 代码中指定

```java
"string.endsWithAny(" + CommonElAllowedVariableEnum.EAVE_MC_ER_CLASS_NAME.getVariableName() + ", '" + test.callgraph.elexample.caller.TestElExampleCaller1.class.getName() + "', '" + test.callgraph.elexample.callee.TestElExampleCallee1.class.getName() + "')"
```

- 表达式示例类名

test.runbycode.el.stringcompare.TestGenCallGraph4EeElErClassNameEndsWithAny

# 7. er_class_name - 判断等于关键字

- 表达式变量说明

调用方完整类名

- 表达式示例说明

在生成向上方法完整调用链时，判断调用类名是否等于指定关键字，忽略匹配的方法调用

- 表达式示例文本

```js
er_class_name == 'test.callgraph.elexample.caller.TestElExampleCaller1'
```

- 表达式示例文本 - 代码中指定

```java
CommonElAllowedVariableEnum.EAVE_MC_ER_CLASS_NAME.getVariableName() + " == '" + test.callgraph.elexample.caller.TestElExampleCaller1.class.getName() + "'"
```

- 表达式示例类名

test.runbycode.el.stringcompare.TestGenCallGraph4EeElErClassNameEquals

# 8. er_class_name - 判断等于多个关键字之一

- 表达式变量说明

调用方完整类名

- 表达式示例说明

在生成向上方法完整调用链时，判断调用类名是否等于指定多个关键字之一，忽略匹配的方法调用

- 表达式示例文本

```js
string.equalsAny(er_class_name, 'test.callgraph.elexample.caller.TestElExampleCaller1', 'test.callgraph.elexample.callee.TestElExampleCallee1')
```

- 表达式示例文本 - 代码中指定

```java
"string.equalsAny(" + CommonElAllowedVariableEnum.EAVE_MC_ER_CLASS_NAME.getVariableName() + ", '" + test.callgraph.elexample.caller.TestElExampleCaller1.class.getName() + "', '" + test.callgraph.elexample.callee.TestElExampleCallee1.class.getName() + "')"
```

- 表达式示例类名

test.runbycode.el.stringcompare.TestGenCallGraph4EeElErClassNameEqualsAny

# 9. er_class_name - 判断多个条件使用或

- 表达式变量说明

调用方完整类名

- 表达式示例说明

在生成向上方法完整调用链时，多个条件使用或运算，判断调用类名是否等于指定多个关键字中的任意一个，忽略匹配的类

- 表达式示例文本

```js
er_class_name == 'test.callgraph.elexample.caller.TestElExampleCaller1' || er_class_name == 'test.callgraph.elexample.callee.TestElExampleCallee1'
```

- 表达式示例文本 - 代码中指定

```java
CommonElAllowedVariableEnum.EAVE_MC_ER_CLASS_NAME.getVariableName() + " == '" + test.callgraph.elexample.caller.TestElExampleCaller1.class.getName() + "' || " + CommonElAllowedVariableEnum.EAVE_MC_ER_CLASS_NAME.getVariableName() + " == '" + test.callgraph.elexample.callee.TestElExampleCallee1.class.getName() + "'"
```

- 表达式示例类名

test.runbycode.el.stringcompare.TestGenCallGraph4EeElErClassNameOr

# 10. er_class_name - 判断以关键字开头

- 表达式变量说明

调用方完整类名

- 表达式示例说明

在生成向上方法完整调用链时，判断调用类名是否以指定关键字开头，忽略匹配的方法调用

- 表达式示例文本

```js
string.startsWith(er_class_name, 'test.callgraph.elexample.caller.TestElExampleCaller1')
```

- 表达式示例文本 - 代码中指定

```java
"string.startsWith(" + CommonElAllowedVariableEnum.EAVE_MC_ER_CLASS_NAME.getVariableName() + ", '" + test.callgraph.elexample.caller.TestElExampleCaller1.class.getName() + "')"
```

- 表达式示例类名

test.runbycode.el.stringcompare.TestGenCallGraph4EeElErClassNameStartsWith

# 11. er_class_name - 判断以多个关键字之一开头

- 表达式变量说明

调用方完整类名

- 表达式示例说明

在生成向上方法完整调用链时，判断调用类名是否以指定多个关键字之一开头，忽略匹配的方法调用

- 表达式示例文本

```js
string.startsWithAny(er_class_name, 'test.callgraph.elexample.caller.TestElExampleCaller1', 'test.callgraph.elexample.callee.TestElExampleCallee1')
```

- 表达式示例文本 - 代码中指定

```java
"string.startsWithAny(" + CommonElAllowedVariableEnum.EAVE_MC_ER_CLASS_NAME.getVariableName() + ", '" + test.callgraph.elexample.caller.TestElExampleCaller1.class.getName() + "', '" + test.callgraph.elexample.callee.TestElExampleCallee1.class.getName() + "')"
```

- 表达式示例类名

test.runbycode.el.stringcompare.TestGenCallGraph4EeElErClassNameStartsWithAny

# 12. er_package_name - 判断包含关键字（忽略大小写）

- 表达式变量说明

调用方完整包名

不会以.结束

- 表达式示例说明

在生成向上方法完整调用链时，判断调用类包名是否包含指定关键字（忽略大小写），忽略匹配的方法调用

- 表达式示例文本

```js
string.containsIC(er_package_name, 'TEST.CALLGRAPH.ELEXAMPLE.CALLER')
```

- 表达式示例文本 - 代码中指定

```java
"string.containsIC(" + CommonElAllowedVariableEnum.EAVE_MC_ER_PACKAGE_NAME.getVariableName() + ", '" + "TEST.CALLGRAPH.ELEXAMPLE.CALLER" + "')"
```

- 表达式示例类名

test.runbycode.el.stringcompare.TestGenCallGraph4EeElErPackageNameContainsIC

# 13. er_package_name - 判断以关键字结尾（忽略大小写）

- 表达式变量说明

调用方完整包名

不会以.结束

- 表达式示例说明

在生成向上方法完整调用链时，判断调用类包名是否以指定关键字结尾（忽略大小写），忽略匹配的方法调用

- 表达式示例文本

```js
string.endsWithIC(er_package_name, 'TEST.CALLGRAPH.ELEXAMPLE.CALLER')
```

- 表达式示例文本 - 代码中指定

```java
"string.endsWithIC(" + CommonElAllowedVariableEnum.EAVE_MC_ER_PACKAGE_NAME.getVariableName() + ", '" + "TEST.CALLGRAPH.ELEXAMPLE.CALLER" + "')"
```

- 表达式示例类名

test.runbycode.el.stringcompare.TestGenCallGraph4EeElErPackageNameEndsWithIC

# 14. er_package_name - 判断等于关键字（忽略大小写）

- 表达式变量说明

调用方完整包名

不会以.结束

- 表达式示例说明

在生成向上方法完整调用链时，判断调用类包名是否等于指定关键字（忽略大小写），忽略匹配的方法调用

- 表达式示例文本

```js
string.equalsIC(er_package_name, 'TEST.CALLGRAPH.ELEXAMPLE.CALLER')
```

- 表达式示例文本 - 代码中指定

```java
"string.equalsIC(" + CommonElAllowedVariableEnum.EAVE_MC_ER_PACKAGE_NAME.getVariableName() + ", '" + "TEST.CALLGRAPH.ELEXAMPLE.CALLER" + "')"
```

- 表达式示例类名

test.runbycode.el.stringcompare.TestGenCallGraph4EeElErPackageNameEqualsIC

# 15. er_package_name - 判断以关键字开头（忽略大小写）

- 表达式变量说明

调用方完整包名

不会以.结束

- 表达式示例说明

在生成向上方法完整调用链时，判断调用类包名是否以指定关键字开头（忽略大小写），忽略匹配的方法调用

- 表达式示例文本

```js
string.startsWithIC(er_package_name, 'TEST.CALLGRAPH.ELEXAMPLE.CALLER')
```

- 表达式示例文本 - 代码中指定

```java
"string.startsWithIC(" + CommonElAllowedVariableEnum.EAVE_MC_ER_PACKAGE_NAME.getVariableName() + ", '" + "TEST.CALLGRAPH.ELEXAMPLE.CALLER" + "')"
```

- 表达式示例类名

test.runbycode.el.stringcompare.TestGenCallGraph4EeElErPackageNameStartsWithIC

