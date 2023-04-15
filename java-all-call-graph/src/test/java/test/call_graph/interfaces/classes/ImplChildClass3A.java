package test.call_graph.interfaces.classes;

import test.call_graph.interfaces.interfaces.InterfaceChild3;

/**
 * @author adrninistrator
 * @date 2022/8/31
 * @description:
 */
public class ImplChildClass3A implements InterfaceChild3 {
    @Override
    public void testSuper1() {
        System.setProperty("e", "f");
    }

    @Override
    public void testChild3() {
        System.getProperty("");
    }

    @Override
    public void testSuper2() {
        System.out.println("");
    }
}
