package test.call_graph.interfaces.classes;

/**
 * @author adrninistrator
 * @date 2022/11/9
 * @description:
 */
public class ExtendsClass2A extends AbstractImplClass2A {
    @Override
    public void testSuper1() {
        System.out.println("");
    }

    @Override
    public void testSuper2() {
        System.out.print("");
    }

    @Override
    public void testChild2() {
        System.err.println("");
    }
}
