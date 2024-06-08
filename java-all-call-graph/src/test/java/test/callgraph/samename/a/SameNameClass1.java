package test.callgraph.samename.a;

/**
 * @author adrninistrator
 * @date 2022/4/28
 * @description:
 */
public class SameNameClass1 {
    public static void test() {
        System.out.println("test");
    }

    public static String testSame1(String aaa1) {
        String bbb1 = "123";
        return aaa1 + bbb1;
    }
}
