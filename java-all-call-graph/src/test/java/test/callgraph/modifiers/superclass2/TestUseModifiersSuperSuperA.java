package test.callgraph.modifiers.superclass2;

import test.callgraph.modifiers.childclass.TestModifiersChildA;

/**
 * @author adrninistrator
 * @date 2025/9/17
 * @description:
 */
public class TestUseModifiersSuperSuperA {

    public void test() {
        TestModifiersChildA testModifiersChildA = new TestModifiersChildA();
        testModifiersChildA.testInSuperSuper();
        System.out.println(testModifiersChildA.flagInSuperSuper);
    }
}
