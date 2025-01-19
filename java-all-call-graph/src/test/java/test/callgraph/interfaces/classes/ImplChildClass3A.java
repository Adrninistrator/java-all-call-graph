package test.callgraph.interfaces.classes;

import test.callgraph.interfaces.interfaces.InterfaceChild3;

import java.util.Collections;
import java.util.List;

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
    public List<String> testChild3List() {
        return Collections.emptyList();
    }

    @Override
    public void testSuper2() {
        System.out.println("");
    }

    @Override
    public void testSuper2D() {
        System.setProperty("", "");
    }

    public void test1() {
        testSuper1();
        testSuper2();
        testSuper2D();
        testChild1();
        testChild3();
    }
}
