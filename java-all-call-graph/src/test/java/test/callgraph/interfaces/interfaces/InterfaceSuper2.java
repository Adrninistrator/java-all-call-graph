package test.callgraph.interfaces.interfaces;

/**
 * @author adrninistrator
 * @date 2022/11/9
 * @description:
 */
public interface InterfaceSuper2 {
    void testSuper2();

    default void testSuper2D() {
        System.out.println("");
    }
}
