package test.callgraph.extendcomplex;

/**
 * @author adrninistrator
 * @date 2022/9/6
 * @description:
 */
public class ChildClassA1 extends AbstractSuperClassA {
    public ChildClassA1() {
        System.out.println("ChildClassA1");
    }

    public void test() {
        entryA();
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
    }

    @Override
    protected void runD() {
        System.out.println("");
    }
}
