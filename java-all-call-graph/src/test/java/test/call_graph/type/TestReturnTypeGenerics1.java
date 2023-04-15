package test.call_graph.type;

import test.call_graph.argument.TestArgument1;
import test.call_graph.argument.TestArgument2;
import test.call_graph.branch.TestBranch1;

import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2023/4/12
 * @description:
 */
public class TestReturnTypeGenerics1 {
    public void testAll() {
        test1();
        test2();
        test3();
        test4();
        test5();
        test6();
    }

    public List<String> test1() {
        return null;
    }

    public Map<Integer, String> test2() {
        return null;
    }

    public List<TestArgument1> test3() {
        return null;
    }

    public Map<Integer, TestArgument1> test4() {
        return null;
    }

    public Map<String, Map<Integer, TestArgument1>> test5() {
        return null;
    }

    public Map<TestArgument1, Map<TestArgument2, TestBranch1>> test6() {
        return null;
    }
}
