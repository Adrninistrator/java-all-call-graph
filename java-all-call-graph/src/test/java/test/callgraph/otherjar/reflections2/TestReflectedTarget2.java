package test.callgraph.otherjar.reflections2;

/**
 * 被反射调用的目标类
 */
public class TestReflectedTarget2 {

    public String sayHello() {
        return "Hello from Reflection!";
    }

    public String greet(String name) {
        return "Hello, " + name + "!";
    }

    public int add(int a, int b) {
        return a + b;
    }

    // 重载方法：方法名相同，参数不同
    public int add(int a, int b, int c) {
        return a + b + c;
    }

    // 重载方法：方法名相同，参数类型不同
    public double add(double a, double b) {
        return a + b;
    }

    // 重载方法：方法名相同，参数数量和类型都不同
    public String add(String a, String b) {
        return a + b;
    }

    // 重载方法：方法名相同，参数类型不同（int + String）
    public String add(int a, String b) {
        return a + b;
    }

    // 重载方法：方法名相同，参数类型不同（String + int）
    public String add(String a, int b) {
        return a + b;
    }

    // 重载方法：方法名相同，参数类型不同（int + double）
    public double add(int a, double b) {
        return a + b;
    }

    // 重载方法：参数类型包含Object（int, Object, double）
    public String process(int a, Object obj, double b) {
        return "int=" + a + ", Object=" + obj + ", double=" + b;
    }

    // 重载方法：参数类型包含Object（Object, Object）
    public String process(Object obj1, Object obj2) {
        return "Object1=" + obj1 + ", Object2=" + obj2;
    }
}
