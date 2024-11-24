package test.callgraph.type;

import test.callgraph.branch.TestBranch1;
import test.callgraph.methodargument.TestArgument1;
import test.callgraph.methodargument.TestArgument2;

import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2023/4/12
 * @description:
 */
public class TestReturnTypeGenerics1 {
    public List<String> testAll() {
        test1();
        test2();
        test3();
        test4();
        test5();
        test6();

        return null;
    }

    public List<String> test1() {
        System.getProperty("");
        return null;
    }

    public Map<Integer, String> test2() {
        System.getProperty("");
        return null;
    }

    public List<TestArgument1> test3() {
        System.getProperty("");
        return null;
    }

    public Map<Integer, TestArgument1> test4() {
        System.getProperty("");
        return null;
    }

    public Map<String, Map<Integer, TestArgument1>> test5() {
        System.getProperty("");
        return null;
    }

    public Map<TestArgument1, Map<TestArgument2, TestBranch1>> test6() {
        System.getProperty("");
        return null;
    }
}
