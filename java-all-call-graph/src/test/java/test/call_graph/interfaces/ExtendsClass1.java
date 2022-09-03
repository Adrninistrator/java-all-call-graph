package test.call_graph.interfaces;

/**
 * @author adrninistrator
 * @date 2022/8/31
 * @description:
 */
public class ExtendsClass1 extends AbstractImplClass1 {
    @Override
    public void testSuper() {
        System.out.println("");
    }

    @Override
    public void testChild() {
        System.err.println("");
    }
}
