package test.callgraph.modifiers.child2;

import test.callgraph.modifiers.childclass.TestModifiersChildA;

/**
 * @author adrninistrator
 * @date 2025/9/17
 * @description:
 */
public class TestModifiersChildChildA extends TestModifiersChildA {

    public void test() {
        testInSuperSuper();

        TestModifiersChildA testModifiersChildA = new TestModifiersChildA();
//        testModifiersChildA.testInSuperSuper();
//        System.out.println(testModifiersChildA.flagInSuperSuper);
    }
}
