package test.call_graph.extend_complex;

/**
 * @author adrninistrator
 * @date 2022/9/6
 * @description:
 */
public class ChildClassA2 extends AbstractSuperClassA {
    public void test() {
        entryA();
        new ChildClassA1().entryA();
    }

    @Override
    protected void runA() {
        System.getProperty("");
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
