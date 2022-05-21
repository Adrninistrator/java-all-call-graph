package test.call_graph.variable_argument;

/**
 * @author adrninistrator
 * @date 2022/5/8
 * @description:
 */
public class TestVANonStatic1 {
    private void test1() {
        test2("123");
        String a = this.test2("aaa");
    }

    private String test2(String a) {
        return a;
    }
}
