package test.callgraph.interfaces.interfaces;

/**
 * @author adrninistrator
 * @date 2022/11/9
 * @description:
 */
public interface InterfaceChild2 extends InterfaceSuper1, InterfaceSuper2 {
    default void testSuper2D() {
        System.getProperty("");
    }
}
