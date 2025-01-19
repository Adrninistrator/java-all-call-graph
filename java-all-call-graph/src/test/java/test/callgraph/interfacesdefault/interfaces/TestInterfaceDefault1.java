package test.callgraph.interfacesdefault.interfaces;

/**
 * @author adrninistrator
 * @date 2024/12/7
 * @description:
 */
public interface TestInterfaceDefault1 {

    default void entry() {
        custom();
    }

    void custom();
}
