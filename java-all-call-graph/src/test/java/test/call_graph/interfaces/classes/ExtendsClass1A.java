package test.call_graph.interfaces.classes;

/**
 * @author adrninistrator
 * @date 2022/8/31
 * @description:
 */
public class ExtendsClass1A extends AbstractImplClass1A {
    @Override
    public void testSuper1() {
        System.out.println("");
    }

    @Override
    public void testChild1() {
        System.err.println("");
    }
}
