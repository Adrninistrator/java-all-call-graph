package test.callgraph.methodargument;

import test.callgraph.branch.TestBranch1;

import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2023/3/7
 * @description:
 */
public class TestArgumentGenerics1 {

    public void testAll(int i, List<TestArgument1> list) {
        TestArgumentGenerics1 testArgumentGenerics1 = new TestArgumentGenerics1();
        testArgumentGenerics1.test1(null);
        testArgumentGenerics1.test2(null);
        testArgumentGenerics1.test3(0, null);
        testArgumentGenerics1.test4(null);
        testArgumentGenerics1.test5(null);
        testArgumentGenerics1.test6(null, null);
        testArgumentGenerics1.test7(null);
    }

    public void test1(List list) {
        System.out.println("");
    }

    public void test2(List<String> list) {
        System.out.println("");
    }

    public void test3(int i, Map<Integer, TestArgument1> map) {
        System.out.println("");
    }

    public void test4(Map<String, Map<Integer, String>> map) {
        System.out.println("");
    }

    public void test5(List<TestArgument1> list) {
        System.out.println("");
    }

    public void test6(Map<String, Map<Integer, TestArgument1>> map, List<TestArgument2> list) {
        System.out.println("");
    }

    public void test7(Map<TestArgument1, Map<TestArgument2, TestBranch1>> map) {
        System.out.println("");
    }

    public void test8(List<String[]> list) {
    }

    public void test9(List<byte[]> list) {
    }
}
