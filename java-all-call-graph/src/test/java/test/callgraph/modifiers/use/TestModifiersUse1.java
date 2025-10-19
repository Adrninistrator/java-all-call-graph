package test.callgraph.modifiers.use;

import test.callgraph.modifiers.childclass.TestModifiersChild1;

/**
 * @author adrninistrator
 * @date 2025/8/7
 * @description:
 */
public class TestModifiersUse1 {

    public void test(){
        TestModifiersChild1 testModifiersChild1= new TestModifiersChild1();
//        testModifiersChild1.protectedInChildProtectedMethod1();
        testModifiersChild1.protectedInChildPublicMethod1();

//        testModifiersChild1.clone();
    }
}
