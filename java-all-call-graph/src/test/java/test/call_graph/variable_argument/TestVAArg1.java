package test.call_graph.variable_argument;

/**
 * @author adrninistrator
 * @date 2022/5/8
 * @description:
 */
public class TestVAArg1 {
    public void test() {
        TestVAArg1 testVAArg1 = new TestVAArg1();
        testVAArg1.test2("a1", System.getProperty("s1"));

        testVAArg1.test2(testVAArg1.test3("t1"), "a2");
    }

    private void test2(String a, String b) {
    }

    private String test3(String a) {
        return a;
    }
}
