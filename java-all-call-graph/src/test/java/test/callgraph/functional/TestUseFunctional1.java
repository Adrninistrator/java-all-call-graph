package test.callgraph.functional;

/**
 * @author adrninistrator
 * @date 2024/1/9
 * @description:
 */
public class TestUseFunctional1 {

    private static final TestFunctional1 TEST_FUNCTIONAL_1 = ((i1, s1) -> {
        String s2 = i1 + "1";
        String s3 = s1 + "2";
        System.out.println(s2 + " " + s3);
        return s2.equals(s3);
    });

    public static void main(String[] args) {
        boolean result = TEST_FUNCTIONAL_1.test(1, "a");
        System.out.println("result: " + result);
    }
}
