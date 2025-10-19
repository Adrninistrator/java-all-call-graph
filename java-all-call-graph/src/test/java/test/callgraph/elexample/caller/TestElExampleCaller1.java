package test.callgraph.elexample.caller;

import test.callgraph.elexample.callee.TestElExampleCallee1;

/**
 * @author adrninistrator
 * @date 2025/9/12
 * @description:
 */
public class TestElExampleCaller1 {

    public void test1(String a) {
        TestElExampleCallee1.testA();
    }
}
