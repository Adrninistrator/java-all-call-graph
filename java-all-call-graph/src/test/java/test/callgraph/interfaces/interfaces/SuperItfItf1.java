package test.callgraph.interfaces.interfaces;

/**
 * @author adrninistrator
 * @date 2024/7/12
 * @description:
 */
public interface SuperItfItf1 {

    default void itfCommon() {
        System.out.println("aaa");
        itfCustom();
    }

    void itfCustom();
}
