package test.callgraph.interfaces.classes;

import test.callgraph.interfaces.interfaces.InterfaceSuper1;

/**
 * @author adrninistrator
 * @date 2022/8/31
 * @description:
 */
public class ImplSuperClass1A implements InterfaceSuper1 {
    @Override
    public void testSuper1() {
        System.getProperty("");
    }
}
