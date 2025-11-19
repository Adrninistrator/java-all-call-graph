# 1. 表达式配置文件说明

- 表达式配置文件名称

_jacg_jar_compatibility/compatibility_check_ignore_class_reference.av

- 表达式配置文件作用

在 Jar兼容性检查时 使用的配置参数

表达式配置参数的说明如下：

若当前配置文件中的表达式执行结果为 true，则跳过将对应的数据写入Jar兼容性检查结果

若表达式执行结果为 false，或未指定表达式，则当前配置不会跳过对应的数据

指定Jar兼容性检查快速模式时是否跳过记录特定的类引用关系

- 表达式配置文件对应的枚举常量名

ElConfigEnum.ECE_COMPATIBILITY_CHECK_IGNORE_CLASS_REFERENCE

以下为支持的表达式变量示例

# 2. er_class_name - Jar兼容性检查快速模式判断引用类名

- 表达式变量说明

调用方完整类名

- 表达式示例说明

Jar兼容性检查快速模式，判断引用类名是否等于指定值，仅处理匹配的类引用关系

- 表达式示例文本

```js
er_class_name != 'test.callgraph.extend.A1_1'
```

- 表达式示例文本 - 代码中指定

```java
CommonElAllowedVariableEnum.EAVE_MC_ER_CLASS_NAME.getVariableName() + " != '" + test.callgraph.extend.A1_1.class.getName() + "'"
```

- 代码中指定表达式示例

```java
configureWrapper.setElConfigText(ElConfigEnum.ECE_COMPATIBILITY_CHECK_IGNORE_CLASS_REFERENCE, 
    CommonElAllowedVariableEnum.EAVE_MC_ER_CLASS_NAME.getVariableName() + " != '" + test.callgraph.extend.A1_1.class.getName() + "'"
);
```

- 表达式示例类名

test.runbycode.el.compatibilityfast.TestCompatibilityCheckFastErClassName

# 3. er_package_name - Jar兼容性检查快速模式判断引用类包名

- 表达式变量说明

调用方完整包名

不会以.结束

- 表达式示例说明

Jar兼容性检查快速模式，判断引用类包名是否等于指定值，仅处理匹配的类引用关系

- 表达式示例文本

```js
er_package_name != 'test.callgraph.extend'
```

- 表达式示例文本 - 代码中指定

```java
CommonElAllowedVariableEnum.EAVE_MC_ER_PACKAGE_NAME.getVariableName() + " != 'test.callgraph.extend'"
```

- 代码中指定表达式示例

```java
configureWrapper.setElConfigText(ElConfigEnum.ECE_COMPATIBILITY_CHECK_IGNORE_CLASS_REFERENCE, 
    CommonElAllowedVariableEnum.EAVE_MC_ER_PACKAGE_NAME.getVariableName() + " != 'test.callgraph.extend'"
);
```

- 表达式示例类名

test.runbycode.el.compatibilityfast.TestCompatibilityCheckFastErPackageName

# 4. er_simple_class_name - Jar兼容性检查快速模式判断引用类简单类名

- 表达式变量说明

调用方简单类名

- 表达式示例说明

Jar兼容性检查快速模式，判断引用类简单类名是否等于指定值，仅处理匹配的类引用关系

- 表达式示例文本

```js
er_simple_class_name != 'A1_1'
```

- 表达式示例文本 - 代码中指定

```java
CommonElAllowedVariableEnum.EAVE_MC_ER_SIMPLE_CLASS_NAME.getVariableName() + " != '" + test.callgraph.extend.A1_1.class.getSimpleName() + "'"
```

- 代码中指定表达式示例

```java
configureWrapper.setElConfigText(ElConfigEnum.ECE_COMPATIBILITY_CHECK_IGNORE_CLASS_REFERENCE, 
    CommonElAllowedVariableEnum.EAVE_MC_ER_SIMPLE_CLASS_NAME.getVariableName() + " != '" + test.callgraph.extend.A1_1.class.getSimpleName() + "'"
);
```

- 表达式示例类名

test.runbycode.el.compatibilityfast.TestCompatibilityCheckFastErSimpleClassName

# 5. ee_class_name - Jar兼容性检查快速模式判断被引用类名

- 表达式变量说明

被调用方完整类名

- 表达式示例说明

Jar兼容性检查快速模式，判断被引用类名是否等于指定值，仅处理匹配的类引用关系

- 表达式示例文本

```js
ee_class_name != 'java.lang.System'
```

- 表达式示例文本 - 代码中指定

```java
CommonElAllowedVariableEnum.EAVE_MC_EE_CLASS_NAME.getVariableName() + " != '" + java.lang.System.class.getName() + "'"
```

- 代码中指定表达式示例

```java
configureWrapper.setElConfigText(ElConfigEnum.ECE_COMPATIBILITY_CHECK_IGNORE_CLASS_REFERENCE, 
    CommonElAllowedVariableEnum.EAVE_MC_EE_CLASS_NAME.getVariableName() + " != '" + java.lang.System.class.getName() + "'"
);
```

- 表达式示例类名

test.runbycode.el.compatibilityfast.TestCompatibilityCheckFast0EeClassName

# 6. ee_package_name - Jar兼容性检查快速模式判断被引用类包名

- 表达式变量说明

被调用方完整包名

不会以.结束

- 表达式示例说明

Jar兼容性检查快速模式，判断被引用类包名是否等于指定值，仅处理匹配的类引用关系

- 表达式示例文本

```js
ee_package_name != 'java.lang'
```

- 表达式示例文本 - 代码中指定

```java
CommonElAllowedVariableEnum.EAVE_MC_EE_PACKAGE_NAME.getVariableName() + " != 'java.lang'"
```

- 代码中指定表达式示例

```java
configureWrapper.setElConfigText(ElConfigEnum.ECE_COMPATIBILITY_CHECK_IGNORE_CLASS_REFERENCE, 
    CommonElAllowedVariableEnum.EAVE_MC_EE_PACKAGE_NAME.getVariableName() + " != 'java.lang'"
);
```

- 表达式示例类名

test.runbycode.el.compatibilityfast.TestCompatibilityCheckFastEePackageName

# 7. ee_simple_class_name - Jar兼容性检查快速模式判断被引用类简单类名

- 表达式变量说明

被调用方简单类名

- 表达式示例说明

Jar兼容性检查快速模式，判断被引用类简单类名是否等于指定值，仅处理匹配的类引用关系

- 表达式示例文本

```js
ee_simple_class_name != 'System'
```

- 表达式示例文本 - 代码中指定

```java
CommonElAllowedVariableEnum.EAVE_MC_EE_SIMPLE_CLASS_NAME.getVariableName() + " != '" + java.lang.System.class.getSimpleName() + "'"
```

- 代码中指定表达式示例

```java
configureWrapper.setElConfigText(ElConfigEnum.ECE_COMPATIBILITY_CHECK_IGNORE_CLASS_REFERENCE, 
    CommonElAllowedVariableEnum.EAVE_MC_EE_SIMPLE_CLASS_NAME.getVariableName() + " != '" + java.lang.System.class.getSimpleName() + "'"
);
```

- 表达式示例类名

test.runbycode.el.compatibilityfast.TestCompatibilityCheckFastEeSimpleClassName

