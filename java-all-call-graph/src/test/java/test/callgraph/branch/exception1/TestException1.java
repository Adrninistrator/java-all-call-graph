package test.callgraph.branch.exception1;

/**
 * @author adrninistrator
 * @date 2026/1/25
 * @description:
 */
public class TestException1 {

    public static void test1(int a) {
        try {
            int b = 1 / (a - 1);
        } catch (ArithmeticException e) {
            throw e;
        }
    }
}
