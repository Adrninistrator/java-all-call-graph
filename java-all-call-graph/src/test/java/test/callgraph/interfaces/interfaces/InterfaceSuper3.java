package test.callgraph.interfaces.interfaces;

/**
 * @author adrninistrator
 * @date 2024/7/11
 * @description:
 */
public interface InterfaceSuper3 {

    default void itfCommon() {
        customer1();
    }

    void customer1();
}
