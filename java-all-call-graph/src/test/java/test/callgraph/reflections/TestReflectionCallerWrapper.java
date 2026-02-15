package test.callgraph.reflections;

import org.junit.Test;

import java.lang.reflect.Method;

/**
 * 使用反射调用目标类的示例
 */
public class TestReflectionCallerWrapper {

    @Test
    public void test() throws Exception {
        Class<?> clazz = TestReflectedTarget.class;
        Object target = clazz.getDeclaredConstructor().newInstance();
        doTest1(clazz, target);
    }

    private void doTest1(Class<?> clazz, Object target) throws Exception {
        // 无参数方法调用
        Method method1 = clazz.getMethod("sayHello");
        String result1 = (String) method1.invoke(target);
        System.out.println(result1);

        // 有参数方法调用 - String参数
        Method method2 = clazz.getMethod("greet", String.class);
        String result2 = (String) method2.invoke(target, "World");
        System.out.println(result2);

        // 有参数方法调用 - 两个int参数
        Method method3 = clazz.getMethod("add", int.class, int.class);
        doTest2(method3, target);

        // 有参数方法调用 - 三个int参数（重载方法）
        Method method4 = clazz.getMethod("add", int.class, int.class, int.class);
        int result4 = (int) method4.invoke(target, 5, 3, 2);
        System.out.println("5 + 3 + 2 = " + result4);

        // 有参数方法调用 - 两个double参数（重载方法）
        Method method5 = clazz.getMethod("add", double.class, double.class);
        double result5 = (double) method5.invoke(target, 5.5, 3.3);
        System.out.println("5.5 + 3.3 = " + result5);

        // 有参数方法调用 - 两个String参数（重载方法）
        Method method6 = clazz.getMethod("add", String.class, String.class);
        String result6 = (String) method6.invoke(target, "Hello", "World");
        System.out.println("String add: " + result6);

        // 有参数方法调用 - String + int参数（参数类型不同）
        Method method7 = clazz.getMethod("add", String.class, int.class);
        String result7 = (String) method7.invoke(target, "Test", 200);
        System.out.println("String + int add: " + result7);
    }

    private void doTest2(Method method, Object target) throws Exception {
        int result3 = (int) method.invoke(target, 5, 3);
        System.out.println("5 + 3 = " + result3);
    }
}
