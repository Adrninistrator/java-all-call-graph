# 1. 作用

java-all-call-graph 项目支持对编译后的 Java 代码进行静态分析，将相关信息写入数据库表，之后可使用 SQL 语句查询静态分析获取的结果

java-all-call-graph 项目地址为 [https://github.com/Adrninistrator/java-all-call-graph](https://github.com/Adrninistrator/java-all-call-graph)

对于反射等方法调用方式，不存在对应的方法调用指令，需要专门处理

以下提供对反射相关的方法调用进行解析的功能，增加反射相关方法调用关系的操作发生在 RunnerWriteDb 类对 Java 代码静态分析并将结果写入数据库阶段

# 2. 解析原生反射调用

## 2.1. 问题背景

在 Java 代码中，反射调用 `Method.invoke()` 动态的方法调用方式。在编译后的字节码中，不存在对应的方法调用指令，因此静态分析工具无法直接识别被调用的实际方法。这导致生成的调用链中缺少反射调用的方法关系。

## 2.2. 支持处理的代码与增加的方法调用关系

当某方法使用 `Class.forName` 获取类对象，并通过 `Class.getMethod` 或 `Class.getDeclaredMethod` 获取方法对象，再调用 `Method.invoke` 执行方法时，会增加对应的方法调用关系。

支持处理的场景：
- 类获取方式：`Class.forName("类名")`
- 方法获取方式：`Class.getMethod("方法名", 参数类型。..)` 或 `Class.getDeclaredMethod("方法名", 参数类型。..)`
- 方法调用方式：`Method.invoke（对象，参数。..)`

不支持的场景：
- 以上方法参数不是常量值
- 以上方法不在同一个方法中调用
- 获取到的方法名或参数类型存在多种可能

示例代码如下：

```java
public class TestReflectionCaller {

    @Test
    public void test() throws Exception {
        Class<?> clazz = Class.forName("test.callgraph.reflections.TestReflectedTarget");
        Object target = clazz.getDeclaredConstructor().newInstance();

        // 无参数方法调用
        Method method1 = clazz.getMethod("sayHello");
        String result1 = (String) method1.invoke(target);

        // 有参数方法调用 - String 参数
        Method method2 = clazz.getMethod("greet", String.class);
        String result2 = (String) method2.invoke(target, "World");

        // 有参数方法调用 - 重载方法（三个 int 参数）
        Method method4 = clazz.getMethod("add", int.class, int.class, int.class);
        int result4 = (int) method4.invoke(target, 5, 3, 2);
    }

    @Test
    public void test2() throws Exception {
        Class<?> clazz = Class.forName("test.callgraph.reflections.TestReflectedTarget");
        Object target = clazz.getDeclaredConstructor().newInstance();

        // 有参数方法调用 - 两个 int 参数（重载方法）
        Method method3 = clazz.getMethod("add", int.class, int.class);
        int result3 = (int) method3.invoke(target, 5, 3);
    }
}
```

对于以上代码，会增加以下方法调用关系：

| 反射调用代码 | 增加的方法调用关系 |
|---|---|
| `method1.invoke(target)` | `TestReflectionCaller:test()` 调用 `TestReflectedTarget:sayHello()` |
| `method2.invoke(target, "World")` | `TestReflectionCaller:test()` 调用 `TestReflectedTarget:greet(java.lang.String)` |
| `method4.invoke(target, 5, 3, 2)` | `TestReflectionCaller:test()` 调用 `TestReflectedTarget:add(int,int,int)` |
| `method3.invoke(target, 5, 3)` | `TestReflectionCaller:test2()` 调用 `TestReflectedTarget:add(int,int)` |

## 2.3. 使用方式

在调用 RunnerWriteDb 类使用的配置参数包装类中，需要进行以下配置：

- _jacg_extensions/javacg2_method_call_extensions.properties

当前配置文件中需要配置 `com.adrninistrator.jacg.extensions.methodcall.JavaCG2ReflectionMethodCallExtension` 类的完整类名

- _jacg_extensions/jacg_method_call_extensions.properties

当前配置文件中需要配置 `com.adrninistrator.jacg.extensions.methodcall.JACGReflectionMethodCallExtension` 类的完整类名

对应代码如下：

```java
        configureWrapper.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_JAVACG2_METHOD_CALL,
                JavaCG2ReflectionMethodCallExtension.class.getName());
        configureWrapper.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_JACG_METHOD_CALL,
                JACGReflectionMethodCallExtension.class.getName());
```

## 2.4. 结果示例

以下是 `TestReflectionCaller:test()` 方法的向下方法调用链（部分内容）：

```
test.callgraph.reflections.TestReflectionCaller:test()
[0]#test.callgraph.reflections.TestReflectionCaller:test()@org.junit.Test
[1]#  [TestReflectionCaller:19]	java.lang.reflect.Method:invoke(java.lang.Object,java.lang.Object[])	!no_callee!
[1]#  [TestReflectionCaller:19]	test.callgraph.reflections.TestReflectedTarget:sayHello()	!no_callee!
[1]#  [TestReflectionCaller:24]	java.lang.reflect.Method:invoke(java.lang.Object,java.lang.Object[])	!no_callee!
[1]#  [TestReflectionCaller:24]	test.callgraph.reflections.TestReflectedTarget:greet(java.lang.String)
[2]#    [TestReflectedTarget:13]	java.lang.StringBuilder:<init>()	!no_callee!
[2]#    [TestReflectedTarget:13]	java.lang.StringBuilder:append(java.lang.String)	!no_callee!
[1]#  [TestReflectionCaller:29]	java.lang.reflect.Method:invoke(java.lang.Object,java.lang.Object[])	!no_callee!
[1]#  [TestReflectionCaller:29]	test.callgraph.reflections.TestReflectedTarget:add(int,int,int)	!no_callee!
...
```

可以看到，通过反射调用的 `sayHello()`、`greet(java.lang.String)`、`add(int,int,int)` 等方法都被正确识别并添加到方法调用链中。

## 2.5. 代码示例

参考 `test.runbycode.reflections.Test0ReflectionsRunnerWriteDb` 类

# 3. 解析反射工具类方法 commons-lang3 MethodUtils

## 3.1. 支持处理的代码与增加的方法调用关系

当某方法调用 org.apache.commons.lang3.reflect.MethodUtils:invokeMethod(java.lang.Object, java.lang.String) 方法时，增加该方法调用参数 1 对象对应类的名称为参数 2 方法的调用关系

示例代码如下：

```java
public class TestReflectionCommonsLang3Invoke {

    @Test
    public void testSuper1() throws InvocationTargetException, NoSuchMethodException, IllegalAccessException {
        TestReflectionCommonsLang3ExampleSuper1 testReflectionCommonsLang3ExampleSuper1 = new TestReflectionCommonsLang3ExampleSuper1();
        MethodUtils.invokeMethod(testReflectionCommonsLang3ExampleSuper1, "test1");
    }

    @Test
    public void testSuper1WithChild() throws InvocationTargetException, NoSuchMethodException, IllegalAccessException {
        TestReflectionCommonsLang3ExampleSuper1 testReflectionCommonsLang3ExampleChild1 = new TestReflectionCommonsLang3ExampleChild1();
        MethodUtils.invokeMethod(testReflectionCommonsLang3ExampleChild1, "test1");
    }
}
```

- MethodUtils.invokeMethod(testReflectionCommonsLang3ExampleSuper1, "test1");

对于该方法，会增加 org.apache.commons.lang3.reflect.MethodUtils:invokeMethod(java.lang.Object, java.lang.String) 方法调用 test.callgraph.reflectioncommonslang3.example.TestReflectionCommonsLang3ExampleSuper1:test1() 方法的关系关系

- MethodUtils.invokeMethod(testReflectionCommonsLang3ExampleChild1, "test1");

对于该方法，会增加 org.apache.commons.lang3.reflect.MethodUtils:invokeMethod(java.lang.Object, java.lang.String) 方法调用 test.callgraph.reflectioncommonslang3.example.TestReflectionCommonsLang3ExampleChild1:test1() 方法的关系关系

## 3.2. 使用方式

在调用 RunnerWriteDb 类使用的配置参数包装类中，需要进行以下配置：

- _jacg_extensions/javacg2_method_call_extensions.properties

当前配置文件中需要配置 test.runbycode.extensions.methodcall.commonslang3methodutils.JavaCG2CommonsLang3MethodUtilsMethodCallExtension 类的完整类名

- _jacg_extensions/jacg_method_call_extensions.properties

当前配置文件中需要配置 test.runbycode.extensions.methodcall.commonslang3methodutils.JACGCommonsLang3MethodUtilsMethodCallExtension 类的完整类名

对应代码如下：

```java
        configureWrapper.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_JAVACG2_METHOD_CALL,
                JavaCG2CommonsLang3MethodUtilsMethodCallExtension.class.getName());
        configureWrapper.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_JACG_METHOD_CALL,
                JACGCommonsLang3MethodUtilsMethodCallExtension.class.getName());
```

## 3.3. 代码示例

参考 test.runbycode.extensions.methodcall.commonslang3methodutils.TestAddMethodCall4CommonsLang3MethodUtils 类

# 4. 解析自定义反射工具类方法

在项目中有自定义反射工具类示例方法，与以上 MethodUtils.invokeMethod 方法的作用与使用方式类似

处理 TestReflectionCustomUtil1 类的方法时需要配置的类如下：

```
test.runbycode.extensions.methodcall.reflectioncustom1.JavaCG2ReflectionCustom1MethodCallExtension
test.runbycode.extensions.methodcall.reflectioncustom1.JACGReflectionCustom1MethodCallExtension
```

处理 TestReflectionCustomUtil1 类的方法时的示例类如下：

test.runbycode.extensions.methodcall.reflectioncustom1.TestAddMethodCall4ReflectionCustom1

处理 TestReflectionCustomUtil1 类的方法时需要配置的类如下：

```
test.runbycode.extensions.methodcall.reflectioncustom2.JACGReflectionCustom2MethodCallExtension
test.runbycode.extensions.methodcall.reflectioncustom2.JavaCG2ReflectionCustom2MethodCallExtension
```

处理 TestReflectionCustomUtil2 类的方法时的示例类如下：

test.runbycode.extensions.methodcall.reflectioncustom2.TestAddMethodCall4ReflectionCustom2
