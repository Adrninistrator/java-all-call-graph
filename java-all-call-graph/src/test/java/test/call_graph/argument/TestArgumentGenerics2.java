package test.call_graph.argument;

/**
 * @author adrninistrator
 * @date 2023/3/7
 * @description:
 */
public class TestArgumentGenerics2 {

    public void testAll() {
        TestArgumentGenerics1 testArgumentGenerics1 = new TestArgumentGenerics1();
        testArgumentGenerics1.test1(null);
        testArgumentGenerics1.test2(null);
        testArgumentGenerics1.test3(null);
        testArgumentGenerics1.test4(null);
        testArgumentGenerics1.test5(null);
    }
}
