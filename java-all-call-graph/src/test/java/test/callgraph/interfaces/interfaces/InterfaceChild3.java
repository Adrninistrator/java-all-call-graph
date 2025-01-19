package test.callgraph.interfaces.interfaces;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2022/11/13
 * @description:
 */
public interface InterfaceChild3 extends InterfaceChild1, InterfaceChild2 {
    void testChild3();

    List<String> testChild3List();
}
