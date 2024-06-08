package test.callgraph.interfaces.classes;

import test.callgraph.interfaces.interfaces.InterfaceChild1;

/**
 * @author adrninistrator
 * @date 2022/8/31
 * @description:
 */
public class ImplChildClass1A implements InterfaceChild1 {
    @Override
    public void testSuper1() {
        System.setProperty("7", "8");
    }

    @Override
    public void testChild1() {
        System.currentTimeMillis();
    }
}
