package test.callgraph.extendsimple;

/**
 * @author adrninistrator
 * @date 2023/5/18
 * @description:
 */
public class SuperClassSA {
    public void fInSuper() {
        testInSuper();
    }

    void fInSuper2() {
        testInSuper();
    }

    private void testInSuper() {
    }
}
