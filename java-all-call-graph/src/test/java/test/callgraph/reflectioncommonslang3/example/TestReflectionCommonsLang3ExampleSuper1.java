package test.callgraph.reflectioncommonslang3.example;

/**
 * @author adrninistrator
 * @date 2025/11/9
 * @description:
 */
public class TestReflectionCommonsLang3ExampleSuper1 {

    public void test1() {
        System.out.println("test1");
    }

    public void test2(String str1) {
        System.out.println("test2 A " + str1);
    }

    public void test2(Integer int1) {
        System.out.println("test2 B " + int1);
    }

    public void test3(String str1, String str2) {
        System.out.println("test3 A " + str1 + " " + str2);
    }

    public void test3(String str1, Integer int1) {
        System.out.println("test3 B " + str1 + " " + int1);
    }
}
