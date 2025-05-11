package test.callgraph.array;

/**
 * @author adrninistrator
 * @date 2025/1/9
 * @description:
 */
public class TestReturnArray1 {

    public static void testUse1() {
        return1();
    }

    public static int[] return1() {
        return2();
        return TestUseArray1.ARRAY1;
    }

    public static int[][] return2() {
        return3();
        return TestUseArray1.ARRAY2;
    }

    public static int[][][] return3() {
        return TestUseArray1.ARRAY3;
    }
}
