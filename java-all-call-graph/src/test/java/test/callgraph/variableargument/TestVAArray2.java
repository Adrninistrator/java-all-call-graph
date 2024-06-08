package test.callgraph.variableargument;

/**
 * @author adrninistrator
 * @date 2022/5/8
 * @description:
 */
public class TestVAArray2 {
    private void test1() {
        String[] array1 = new String[]{};
        String s1 = array1[0];

        String[][] array2 = new String[][]{};
        String[] array21 = array2[0];
    }

    private void test2() {
        TestVAArray2[] array1 = new TestVAArray2[]{};
        TestVAArray2 t1 = array1[0];

        TestVAArray2[][] array2 = new TestVAArray2[][]{};
        TestVAArray2[] array21 = array2[0];
    }
}
