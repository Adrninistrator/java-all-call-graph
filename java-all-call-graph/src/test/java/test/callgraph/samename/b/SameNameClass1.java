package test.callgraph.samename.b;

/**
 * @author adrninistrator
 * @date 2022/4/28
 * @description:
 */
public class SameNameClass1 {
    public static void test() {
        System.out.println("test");
    }

    public static String testSame1(String aaa2) {
        String bbb2 = "123";
        return aaa2 + bbb2;
    }
}
