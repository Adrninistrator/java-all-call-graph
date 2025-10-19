package test.callgraph.modifiers.superclass;

import test.callgraph.modifiers.childclass.TestModifiersChildA;
import test.callgraph.modifiers.superclass2.TestModifiersSuperSuperA;

/**
 * @author adrninistrator
 * @date 2025/9/17
 * @description:
 */
public class TestModifiersSuperA extends TestModifiersSuperSuperA {

    public void test() {
        TestModifiersChildA testModifiersChildA = new TestModifiersChildA();
        testModifiersChildA.testInSuperSuper();
        System.out.println(testModifiersChildA.flagInSuperSuper);
    }
}
