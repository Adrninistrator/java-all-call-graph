package test.callgraph.elexample.callee;

/**
 * @author adrninistrator
 * @date 2025/9/12
 * @description:
 */
public class TestElExampleCallee1 {

    public static void testA() {
        testB();
    }

    private static void testB() {
        testC();
    }

    private static void testC() {
    }
}
