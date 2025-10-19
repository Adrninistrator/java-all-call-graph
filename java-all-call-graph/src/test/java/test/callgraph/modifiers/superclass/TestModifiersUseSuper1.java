package test.callgraph.modifiers.superclass;

import test.callgraph.modifiers.childclass.TestModifiersChild1;

/**
 * @author adrninistrator
 * @date 2025/8/7
 * @description:
 */
public class TestModifiersUseSuper1 {

    public void testField1() {
        TestModifiersSuper1 testModifiersSuper1 = new TestModifiersSuper1();
        System.out.println(testModifiersSuper1.flagInSuper);

        TestModifiersChild1 testModifiersChild1 = new TestModifiersChild1();
        System.out.println(testModifiersChild1.flagInSuper);
//        System.out.println(testModifiersChild1.flagInChild);
    }

    public void testMethod1() {
        TestModifiersSuper1 testModifiersSuper1 = new TestModifiersSuper1();
        testModifiersSuper1.protectedInSuperMethod1();
        testModifiersSuper1.protectedInChildProtectedMethod1();
        testModifiersSuper1.protectedInChildPublicMethod1();

        TestModifiersChild1 testModifiersChild1 = new TestModifiersChild1();
        testModifiersChild1.protectedInSuperMethod1();
//        testModifiersChild1.protectedInChildProtectedMethod1();
        testModifiersChild1.protectedInChildPublicMethod1();
    }
}
