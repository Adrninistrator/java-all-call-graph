package test.callgraph.extendcomplex;

/**
 * @author adrninistrator
 * @date 2022/9/6
 * @description:
 */
public class ChildClassB1 extends AbstractSuperClassB {
    public void test() {
        entryA();
        new ChildClassA2().entryA();
    }

    @Override
    protected void runA() {
        System.out.println("");
    }

    @Override
    protected void runB() {
        System.out.println("");
    }

    @Override
    protected void runC() {
        System.out.println("");
        new ChildClassB2().entryB();
    }

    @Override
    protected void runD() {
        System.out.println("");
    }
}
