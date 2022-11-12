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

    private void test1() {
        int i = (int) System.currentTimeMillis() % 10;
        int i2 = (int) System.currentTimeMillis() % 10;
        int i3 = (int) System.currentTimeMillis() % 10;
        String s1 = (i3 == 3 ? (i2 == 5 ? "a" : "b") : (i == 7 ? "c" : "d"));

        String s2 = "--";
        if (i3 == 1) {
            s2 = (i == 1 ? "aa" : "bb");
        } else if (i3 == 2) {
            s2 = (i == 2 ? "cc" : "dd");
        } else if (i3 == 3) {
            s2 = (i == 3 ? "ee" : "ff");
        } else if (i3 == 4) {
            s2 = (i == 4 ? "gg" : "hh");
        }

        System.setProperty(s1, s2);
    }
}
