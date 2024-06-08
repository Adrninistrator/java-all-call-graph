package test.callgraph.variableargument;

/**
 * @author adrninistrator
 * @date 2022/5/8
 * @description:
 */
public class TestVAArray1 {
    private void test1() {
        int[] array1 = new int[]{};
        int[] array2 = new int[]{1};
        System.out.println(array1[0]);
    }

    private void test2() {
        boolean[][] array1 = new boolean[][]{};
        boolean[][] array2 = new boolean[][]{new boolean[]{false}, new boolean[]{true, false}};
        boolean[][][] array3a = new boolean[][][]{};
        boolean[][][] array3b = new boolean[1][2][3];
        boolean[][][][] array4a = new boolean[][][][]{};
        boolean[][][][] array4b = new boolean[1][2][3][4];

        boolean[] array1a = array1[0];
        System.out.println(array1a.length);
        System.out.println(array1a[0]);
    }

    private void test3() {
        TestVAArg1[][] array1 = new TestVAArg1[][]{};
        TestVAArg1[][] array2 = new TestVAArg1[][]{new TestVAArg1[]{new TestVAArg1()}, new TestVAArg1[]{new TestVAArg1(), new TestVAArg1()}};
        TestVAArg1[][][] array3a = new TestVAArg1[][][]{};
        TestVAArg1[][][] array3b = new TestVAArg1[1][2][3];
        TestVAArg1[][][][] array4a = new TestVAArg1[][][][]{};
        TestVAArg1[][][][] array4b = new TestVAArg1[1][2][3][4];

        TestVAArg1[] array1a = array1[0];
        System.out.println(array1a.length);
        System.out.println(array1a[0]);
    }
}
