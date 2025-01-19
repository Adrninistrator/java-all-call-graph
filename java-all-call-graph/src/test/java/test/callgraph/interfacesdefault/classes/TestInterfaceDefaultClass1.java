package test.callgraph.interfacesdefault.classes;

import test.callgraph.interfacesdefault.interfaces.TestInterfaceDefault1;

/**
 * @author adrninistrator
 * @date 2024/12/7
 * @description:
 */
public class TestInterfaceDefaultClass1 implements TestInterfaceDefault1 {
    @Override
    public void custom() {
        System.getenv("");
    }
}
