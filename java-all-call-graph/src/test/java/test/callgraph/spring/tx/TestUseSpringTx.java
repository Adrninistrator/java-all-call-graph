package test.callgraph.spring.tx;

/**
 * @author adrninistrator
 * @date 2025/11/23
 * @description:
 */
public class TestUseSpringTx {

    public void test() {
        TestSpringTx testSpringTx = new TestSpringTx();
        testSpringTx.test1();
        testSpringTx.test2();
    }
}
