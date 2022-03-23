package test.call_graph.argument;

/**
 * @author adrninistrator
 * @date 2021/11/1
 * @description:
 */
public class TestArgument2 {

    public static void test() {
        int i = (int) System.currentTimeMillis() % 10;
        String s1 = (i == 7 ? "a" : "b");

        String s2 = "aa";
        int ii = 1;
        if (i == 7) {
            s2 = "bb";
        }

        testString(s1);
        testString(s2);
    }

    private static void testString(String s) {
    }
}
