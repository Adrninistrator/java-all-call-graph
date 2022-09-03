package test.call_graph.interfaces;

/**
 * @author adrninistrator
 * @date 2022/8/31
 * @description:
 */
public class ImplChildClass1 implements InterfaceChild1 {
    @Override
    public void testSuper() {
        System.setProperty("", "");
    }

    @Override
    public void testChild() {
        System.currentTimeMillis();
    }
}
