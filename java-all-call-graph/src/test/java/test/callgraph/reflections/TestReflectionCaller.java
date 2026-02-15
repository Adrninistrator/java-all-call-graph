package test.callgraph.reflections;

import org.junit.Test;

import java.lang.reflect.Method;

/**
 * 使用反射调用目标类的示例
 */
public class TestReflectionCaller {

    @Test
    public void test() throws Exception {
        Class<?> clazz = Class.forName("test.callgraph.reflections.TestReflectedTarget");
        Object target = clazz.getDeclaredConstructor().newInstance();

        // 无参数方法调用
        Method method1 = clazz.getMethod("sayHello");
        String result1 = (String) method1.invoke(target);
        System.out.println(result1);

        // 有参数方法调用 - String参数
        Method method2 = clazz.getMethod("greet", String.class);
        String result2 = (String) method2.invoke(target, "World");
        System.out.println(result2);

        // 有参数方法调用 - 三个int参数（重载方法）
        Method method4 = clazz.getMethod("add", int.class, int.class, int.class);
        int result4 = (int) method4.invoke(target, 5, 3, 2);
        System.out.println("5 + 3 + 2 = " + result4);

        // 有参数方法调用 - 两个double参数（重载方法，参数类型不同）
        Method method5 = clazz.getMethod("add", double.class, double.class);
        double result5 = (double) method5.invoke(target, 5.5, 3.3);
        System.out.println("5.5 + 3.3 = " + result5);

        // 有参数方法调用 - 两个String参数（重载方法，参数类型不同）
        Method method6 = clazz.getMethod("add", String.class, String.class);
        String result6 = (String) method6.invoke(target, "Hello", "World");
        System.out.println("String add: " + result6);

        // 有参数方法调用 - int + String参数（参数类型不同）
        Method method7 = clazz.getMethod("add", int.class, String.class);
        String result7 = (String) method7.invoke(target, 100, "Test");
        System.out.println("int + String add: " + result7);

        // 有参数方法调用 - int + double参数（参数类型不同）
        Method method8 = clazz.getMethod("add", int.class, double.class);
        double result8 = (double) method8.invoke(target, 5, 3.5);
        System.out.println("int + double add: " + result8);
    }

    @Test
    public void test2() throws Exception {
//        Class<?> clazz = TestReflectedTarget.class;
        Class<?> clazz = Class.forName("test.callgraph.reflections.TestReflectedTarget");
        Object target = clazz.getDeclaredConstructor().newInstance();

        // 有参数方法调用 - 两个int参数
        Method method3 = clazz.getMethod("add", int.class, int.class);
        int result3 = (int) method3.invoke(target, 5, 3);
        System.out.println("5 + 3 = " + result3);
    }

    @Test
    public void test3() throws Exception {
//        Class<?> clazz = TestReflectedTarget.class;
        Class<?> clazz = Class.forName("test.callgraph.reflections.TestReflectedTarget");
        Object target = clazz.getDeclaredConstructor().newInstance();

        // 有参数方法调用
        Method method8 = clazz.getMethod("add", int.class, double.class);
        double result8 = (double) method8.invoke(target, 5, 3.5);
        System.out.println("int + double add: " + result8);
    }

    @Test
    public void test4() throws Exception {
        Class<?> clazz = Class.forName("test.callgraph.reflections.TestReflectedTarget");
        Object target = clazz.getDeclaredConstructor().newInstance();

        // 有参数方法调用 - int, Object, double参数
        Method method = clazz.getMethod("process", int.class, Object.class, double.class);
        String result = (String) method.invoke(target, 10, "TestObject", 3.14);
        System.out.println("int + Object + double process: " + result);
    }

    @Test
    public void test5() throws Exception {
        Class<?> clazz = Class.forName("test.callgraph.otherjar.reflections2.TestReflectedTarget2");
        Object target = clazz.getDeclaredConstructor().newInstance();

        // 有参数方法调用 - Object, Object参数
        Method method = clazz.getMethod("process", Object.class, Object.class);
        String result = (String) method.invoke(target, "First", "Second");
        System.out.println("Object + Object process: " + result);
    }
}
