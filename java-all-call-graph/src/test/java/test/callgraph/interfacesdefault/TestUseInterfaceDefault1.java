package test.callgraph.interfacesdefault;

import test.callgraph.interfacesdefault.classes.TestInterfaceDefaultClass1;
import test.callgraph.interfacesdefault.interfaces.TestInterfaceDefault1;

/**
 * @author adrninistrator
 * @date 2024/12/7
 * @description:
 */
public class TestUseInterfaceDefault1 {

    public void test1() {
        TestInterfaceDefault1 testInterfaceDefault1 = new TestInterfaceDefaultClass1();
        testInterfaceDefault1.entry();
    }

    public void test2() {
        TestInterfaceDefaultClass1 testInterfaceDefaultClass1 = new TestInterfaceDefaultClass1();
        testInterfaceDefaultClass1.entry();
    }

    public void test3() {
        TestInterfaceDefault1 testInterfaceDefault1 = null;
        testInterfaceDefault1.entry();
    }
}
