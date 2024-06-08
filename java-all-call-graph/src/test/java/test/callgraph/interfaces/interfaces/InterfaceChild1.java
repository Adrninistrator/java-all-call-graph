package test.callgraph.interfaces.interfaces;

/**
 * @author adrninistrator
 * @date 2022/8/31
 * @description:
 */
public interface InterfaceChild1 extends InterfaceSuper1 {
    default void testChild1() {
        System.out.println("abc");
    }
}
