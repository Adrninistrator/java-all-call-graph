package test.callgraph.polymorphism.childoverride;

/**
 * @author adrninistrator
 * @date 2023/11/22
 * @description:
 */
public class TestUseChildOverride {

    private ChildOverrideChildAA childOverrideChildAAGlobal;

    public void test1() {
        ChildOverrideSuper childOverrideChildA = new ChildOverrideChildA();
        childOverrideChildA.exec();
    }

    public void test2() {
        ChildOverrideSuper childOverrideChildAA = new ChildOverrideChildAA();
        childOverrideChildAA.exec();
    }

    public void test3() {
        ChildOverrideChildAA childOverrideChildAA = new ChildOverrideChildAA();
        childOverrideChildAA.exec2();
    }

    public void test4() {
        childOverrideChildAAGlobal.exec();
        childOverrideChildAAGlobal.exec2();
    }
}
