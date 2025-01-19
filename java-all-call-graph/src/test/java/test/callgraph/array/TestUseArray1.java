package test.callgraph.array;

/**
 * @author adrninistrator
 * @date 2025/1/9
 * @description:
 */
public class TestUseArray1 {

    public static final int[] ARRAY1 = new int[]{1};
    public static final int[][] ARRAY2 = new int[][]{new int[]{1, 2}, new int[]{3, 4}};
    public static final int[][][] ARRAY3 = new int[][][]{new int[][]{new int[]{1, 2, 3}, new int[]{4, 5, 6}}};

    private void test1() {
        useArray(new int[]{});
        useArray(new int[1]);
        useArray(ARRAY1);
        useArray(TestReturnArray1.return1());
    }

    private void test2() {
        useArray(new int[][]{});
        useArray(new int[1][2]);
        useArray(ARRAY2);
        useArray(TestReturnArray1.return2());
    }

    private void test3() {
        useArray(new int[][][]{});
        useArray(new int[1][2][3]);
        useArray(ARRAY3);
        useArray(TestReturnArray1.return3());
    }

    private void testUse0() {
        useArray(new int[]{1}[0]);
        int[] array1 = new int[1];
        useArray(array1[0]);
        useArray(ARRAY1[0]);
        useArray(TestReturnArray1.return1()[0]);

        useArray(new int[][]{{1, 2}, {3, 4}}[0][0]);
        int[][] array2 = new int[1][2];
        useArray(array2[0][0]);
        useArray(ARRAY2[0][0]);
        useArray(TestReturnArray1.return2()[0][0]);

        useArray(new int[][][]{{{1, 2, 3}, {4, 5, 6}}}[0][0][0]);
        int[][][] array3 = new int[1][2][3];
        useArray(array3[0][0][0]);
        useArray(ARRAY3[0][0][0]);
        useArray(TestReturnArray1.return3()[0][0][0]);
    }

    private void testUse1() {
        useArray(new int[][]{{1, 2}, {3, 4}}[0]);
        int[][] array2 = new int[1][2];
        useArray(array2[0]);
        useArray(ARRAY2[0]);
        useArray(TestReturnArray1.return2()[0]);

        useArray(new int[][][]{{{1, 2, 3}, {4, 5, 6}}}[0][0]);
        int[][][] array3 = new int[1][2][3];
        useArray(array3[0][0]);
        useArray(ARRAY3[0][0]);
        useArray(TestReturnArray1.return3()[0][0]);
    }

    private void testUse2() {
        useArray(new int[][][]{{{1, 2, 3}, {4, 5, 6}}}[0]);
        int[][][] array3 = new int[1][2][3];
        useArray(array3[0]);
        useArray(ARRAY3[0]);
        useArray(TestReturnArray1.return3()[0]);
    }

    private void useArray(Object obj) {
    }
}
