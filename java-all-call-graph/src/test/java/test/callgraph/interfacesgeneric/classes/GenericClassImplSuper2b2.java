package test.callgraph.interfacesgeneric.classes;

/**
 * @author adrninistrator
 * @date 2023/8/8
 * @description:
 */
public class GenericClassImplSuper2b2 extends GenericAbstractSuper2<String[], byte[]> {

    @Override
    public void test(String[] a1, byte[] a2, String str, int i) {
        doTest();
    }

    private void doTest() {
    }
}
