package test.callgraph.modifiers.use;

import test.callgraph.modifiers.superclass.TestModifiersSuper1;

/**
 * @author adrninistrator
 * @date 2025/8/14
 * @description:
 */
public class TestModifiersUseExtendsSuper1 extends TestModifiersSuper1 {

    public void testField1() {
        System.out.println(flagInSuper);
        System.out.println(super.flagInSuper);

//        TestModifiersSuper1 testModifiersSuper1 = new TestModifiersSuper1();
//        System.out.println(testModifiersSuper1.flag);
//
//        TestModifiersChild1 testModifiersChild1 = new TestModifiersChild1();
//        System.out.println(testModifiersChild1.flag);
    }

    public void testMethod1() {
        protectedInSuperMethod1();
        super.protectedInSuperMethod1();

//        TestModifiersSuper1 testModifiersSuper1 = new TestModifiersSuper1();
//        testModifiersSuper1.protectedInSuperMethod1();
//
//        TestModifiersChild1 testModifiersChild1 = new TestModifiersChild1();
//        testModifiersChild1.protectedInSuperMethod1();
    }
}
