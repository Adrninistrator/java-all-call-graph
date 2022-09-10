package test.call_graph.extend_complex.only_class;

/**
 * @author adrninistrator
 * @date 2022/9/6
 * @description:
 */
public class ChildClassA2 extends AbstractSuperClassA {
    public void test() {
        entryA();
    }

    @Override
    protected void runA() {
        System.getProperty("");
    }
}
