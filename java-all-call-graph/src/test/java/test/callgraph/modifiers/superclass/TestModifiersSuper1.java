package test.callgraph.modifiers.superclass;

/**
 * @author adrninistrator
 * @date 2025/8/7
 * @description:
 */
public class TestModifiersSuper1 {

    protected String flagInSuper = "";

    protected void protectedInSuperMethod1() {
    }

    protected void protectedInChildProtectedMethod1() {
    }

    protected void protectedInChildPublicMethod1() {
    }

//    public void test() {
//        TestModifiersChildA testModifiersChildA = new TestModifiersChildA();
//        testModifiersChildA.testInSuperSuper();
//        System.out.println(testModifiersChildA.flagInSuperSuper);
//    }
}
