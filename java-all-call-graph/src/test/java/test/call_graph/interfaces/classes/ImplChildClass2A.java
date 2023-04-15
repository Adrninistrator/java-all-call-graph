package test.call_graph.interfaces.classes;

import test.call_graph.interfaces.interfaces.InterfaceChild2;

/**
 * @author adrninistrator
 * @date 2022/11/9
 * @description:
 */
public class ImplChildClass2A implements InterfaceChild2 {
    @Override
    public void testSuper1() {
        System.setProperty("c", "d");
    }

    @Override
    public void testSuper2() {
        System.getProperty("", "");
    }
}
