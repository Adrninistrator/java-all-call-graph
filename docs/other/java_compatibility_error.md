# 说明

在升级JDK，或Java依赖库版本时，有可能会遇到兼容性问题，相关异常及提示信息如下

# 异常信息

## 汇总后的异常

```
java.lang.IllegalAccessError
java.lang.IncompatibleClassChangeError
java.lang.NoClassDefFoundError
java.lang.NoSuchFieldError
java.lang.NoSuchMethodError
```

## 被调用类不存在

- jdk_8

java.lang.NoClassDefFoundError: test/compatibility/CompatibilityCallee1

- jdk_17

java.lang.NoClassDefFoundError: test/compatibility/CompatibilityCallee1

- jdk_21

java.lang.NoClassDefFoundError: test/compatibility/CompatibilityCallee1

## 被调用方法不存在（方法被删除、方法名称变化、方法参数数量或类型变化）

- jdk_8

java.lang.NoSuchMethodError: test.compatibility.CompatibilityCallee2.staticMethod1()V

- jdk_17

java.lang.NoSuchMethodError: 'void test.compatibility.CompatibilityCallee2.staticMethod1()'

- jdk_21

java.lang.NoSuchMethodError: 'void test.compatibility.CompatibilityCallee2.staticMethod1()'

## 被调用方法由public变成private

- jdk_8

java.lang.NoSuchMethodError: test.compatibility.CompatibilityCallee2.runtimePrivateStaticMethod1()V

- jdk_17

java.lang.NoSuchMethodError: 'void test.compatibility.CompatibilityCallee2.runtimePrivateStaticMethod1()'

- jdk_21

java.lang.NoSuchMethodError: 'void test.compatibility.CompatibilityCallee2.runtimePrivateStaticMethod1()'

## 被调用方法由protected变成private

- jdk_8

java.lang.NoSuchMethodError: test.compatibility.CompatibilityCallee3.runtimePrivateStaticMethod1()V

- jdk_17

java.lang.NoSuchMethodError: 'void test.compatibility.CompatibilityCallee3.runtimePrivateStaticMethod1()'

- jdk_21

java.lang.NoSuchMethodError: 'void test.compatibility.CompatibilityCallee3.runtimePrivateStaticMethod1()'

## 被调用方法由非静态变成静态

- jdk_8

java.lang.IncompatibleClassChangeError: Expecting non-static method test.compatibility.CompatibilityCallee2.runtimeNonStaticToStaticMethod1()V

- jdk_17

java.lang.IncompatibleClassChangeError: Expecting non-static method 'void test.compatibility.CompatibilityCallee2.runtimeNonStaticToStaticMethod1()'

- jdk_21

java.lang.IncompatibleClassChangeError: Expecting non-static method 'void test.compatibility.CompatibilityCallee2.runtimeNonStaticToStaticMethod1()'

## 被调用方法由静态变成非静态

- jdk_8

java.lang.IncompatibleClassChangeError: Expected static method test.compatibility.CompatibilityCallee2.runtimeStaticToNonStaticMethod1()V

- jdk_17

java.lang.IncompatibleClassChangeError: Expected static method 'void test.compatibility.CompatibilityCallee2.runtimeStaticToNonStaticMethod1()'

- jdk_21

java.lang.IncompatibleClassChangeError: Expected static method 'void test.compatibility.CompatibilityCallee2.runtimeStaticToNonStaticMethod1()'

## 被调用方法返回类型改变

- jdk_8

java.lang.NoSuchMethodError: test.compatibility.CompatibilityCallee2.runtimeChangeReturnTypeMethod1()Ljava/lang/String;

- jdk_17

java.lang.NoSuchMethodError: 'java.lang.String test.compatibility.CompatibilityCallee2.runtimeChangeReturnTypeMethod1()'

- jdk_21

java.lang.NoSuchMethodError: 'java.lang.String test.compatibility.CompatibilityCallee2.runtimeChangeReturnTypeMethod1()'

## 使用的字段不存在（字段被删除、字段名称变化）

- jdk_8

java.lang.NoSuchFieldError: STATIC_FIELD1

- jdk_17

java.lang.NoSuchFieldError: STATIC_FIELD1

- jdk_21

java.lang.NoSuchFieldError: Class test.compatibility.CompatibilityCallee2 does not have member field 'java.lang.Object STATIC_FIELD1'

## 使用的字段由public变成private

- jdk_8

java.lang.IllegalAccessError: tried to access field test.compatibility.CompatibilityCallee2.RUNTIME_PRIVATE_STATIC_FIELD1 from class test.compatibility.CompatibilityCaller

- jdk_17

java.lang.IllegalAccessError: class test.compatibility.CompatibilityCaller tried to access private field test.compatibility.CompatibilityCallee2.RUNTIME_PRIVATE_STATIC_FIELD1 (test.compatibility.CompatibilityCaller and test.compatibility.CompatibilityCallee2 are in unnamed module of loader 'app')

- jdk_21

java.lang.IllegalAccessError: class test.compatibility.CompatibilityCaller tried to access private field test.compatibility.CompatibilityCallee2.RUNTIME_PRIVATE_STATIC_FIELD1 (test.compatibility.CompatibilityCaller and test.compatibility.CompatibilityCallee2 are in unnamed module of loader 'app')

## 使用的字段由非静态变成静态

- jdk_8

java.lang.IncompatibleClassChangeError: Expected non-static field test.compatibility.CompatibilityCallee2.runtimeStaticField1

- jdk_17

java.lang.IncompatibleClassChangeError: Expected non-static field test.compatibility.CompatibilityCallee2.runtimeStaticField1

- jdk_21

java.lang.IncompatibleClassChangeError: Expected non-static field test.compatibility.CompatibilityCallee2.runtimeStaticField1

## 使用的字段由静态变成非静态

- jdk_8

java.lang.IncompatibleClassChangeError: Expected static field test.compatibility.CompatibilityCallee2.RUNTIME_NON_STATIC_FIELD1

- jdk_17

java.lang.IncompatibleClassChangeError: Expected static field test.compatibility.CompatibilityCallee2.RUNTIME_NON_STATIC_FIELD1

- jdk_21

java.lang.IncompatibleClassChangeError: Expected static field test.compatibility.CompatibilityCallee2.RUNTIME_NON_STATIC_FIELD1

## 使用的字段由protected变成private

- jdk_8

java.lang.IllegalAccessError: tried to access field test.compatibility.CompatibilityCallee3.RUNTIME_PRIVATE_STATIC_FIELD1 from class test.compatibility.CompatibilityCaller

- jdk_17

java.lang.IllegalAccessError: class test.compatibility.CompatibilityCaller tried to access private field test.compatibility.CompatibilityCallee3.RUNTIME_PRIVATE_STATIC_FIELD1 (test.compatibility.CompatibilityCaller and test.compatibility.CompatibilityCallee3 are in unnamed module of loader 'app')

- jdk_21

java.lang.IllegalAccessError: class test.compatibility.CompatibilityCaller tried to access private field test.compatibility.CompatibilityCallee3.RUNTIME_PRIVATE_STATIC_FIELD1 (test.compatibility.CompatibilityCaller and test.compatibility.CompatibilityCallee3 are in unnamed module of loader 'app')

## 使用的字段类型改变

- jdk_8

java.lang.NoSuchFieldError: runtimeChangeTypeField1

- jdk_17

java.lang.NoSuchFieldError: runtimeChangeTypeField1

- jdk_21

java.lang.NoSuchFieldError: Class test.compatibility.CompatibilityCallee2 does not have member field 'java.lang.Object runtimeChangeTypeField1'

## 使用的枚举常量值找不到

- jdk_8

java.lang.NoSuchFieldError: ENUM_1

- jdk_17

java.lang.NoSuchFieldError: ENUM_1

- jdk_21

java.lang.NoSuchFieldError: Class test.compatibility.CompatibilityEnum1 does not have member field 'test.compatibility.CompatibilityEnum1 ENUM_1'

