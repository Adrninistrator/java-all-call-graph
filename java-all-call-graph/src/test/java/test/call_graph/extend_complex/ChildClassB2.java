package test.call_graph.extend_complex;

/**
 * @author adrninistrator
 * @date 2022/9/6
 * @description:
 */
public class ChildClassB2 extends AbstractSuperClassB {
    public void test() {
        entryA();
    }

    @Override
    protected void runB() {
        System.identityHashCode(this);
        new ChildClassB1().entryA();
        new ChildClassB1().entryB();
    }
}
