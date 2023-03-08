package test.call_graph.argument;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2023/3/7
 * @description:
 */
public class TestArgumentGenerics1 {

    public void testAll() {
        TestArgumentGenerics1 testArgumentGenerics1 = new TestArgumentGenerics1();
        testArgumentGenerics1.test1(null);
        testArgumentGenerics1.test2(null);
        testArgumentGenerics1.test3(null);
        testArgumentGenerics1.test4(null);
        testArgumentGenerics1.test5(null);
    }

    public void test1(List list) {
        System.out.println("");
    }

    public void test2(List<String> list) {
        System.out.println("");
    }

    public void test3(Map<Integer, String> map) {
        System.out.println("");
    }

    public void test4(Map<String, Map<Integer, String>> map) {
        System.out.println("");
    }

    public void test5(List<TestArgument1> list) {
        System.out.println("");
    }
}
