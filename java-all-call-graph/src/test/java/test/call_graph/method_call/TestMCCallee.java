package test.call_graph.method_call;

import java.math.BigDecimal;

/**
 * @author adrninistrator
 * @date 2021/8/9
 * @description:
 */
public class TestMCCallee {

    public static final String TEST_STRING = "test_string";

    @Deprecated
    public static void test1(String str1) {
    }

    public void test2(String str1, String str2) {
        int a;
        int b = 3;
        System.out.println(b);
    }

    public static void test3(String str1, String str2, String str3) {
    }

    public static String run(int a, String b, BigDecimal c) {
        String s1 = "";
        String s2;
        System.out.println(s1);
        return "";
    }
}
