package test.callgraph.methodreturn;

import test.callgraph.branch.TestBranch1;
import test.callgraph.methodargument.TestArgument1;
import test.callgraph.methodargument.TestArgument2;

import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2024/10/27
 * @description:
 */
public class TestMethodReturnGenericsType1 {

    public int int1() {
        return 0;
    }

    public String str1() {
        return null;
    }

    public List test1() {
        return null;
    }

    public List<String> test2() {
        return null;
    }

    public Map<Integer, TestArgument1> test3() {
        System.getProperty("");
        return null;
    }

    public Map<String, Map<Integer, String>> test4() {
        return null;
    }

    public List<TestArgument1> test5() {
        return null;
    }

    public Map<String, Map<Integer, TestArgument1>> test6() {
        return null;
    }

    public Map<TestArgument1, Map<TestArgument2, TestBranch1>> test7() {
        return null;
    }

    public List<String[]> test8() {
        return null;
    }

    public List<char[]> test9() {
        return null;
    }

    public Map<String, Map<String, List<Boolean>>> test10() {
        return null;
    }
}
