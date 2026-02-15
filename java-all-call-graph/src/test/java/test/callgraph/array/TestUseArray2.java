package test.callgraph.array;

import java.math.BigDecimal;

/**
 * @author adrninistrator
 * @date 2025/1/9
 * @description:
 */
public class TestUseArray2 {

    private void test0A() {
        useArray(new int[]{}[0]);
        useArray(new float[]{}[0]);
        useArray(new long[]{}[0]);
        useArray(new double[]{}[0]);
        useArray(new BigDecimal[]{}[0]);
        useArray(new byte[]{}[0]);
        useArray(new boolean[]{}[0]);
        useArray(new char[]{}[0]);
        useArray(new short[]{}[0]);
    }

    private void test0B() {
        useArray(new int[][]{}[0][0]);
        useArray(new float[][]{}[0][0]);
        useArray(new long[][]{}[0][0]);
        useArray(new double[][]{}[0][0]);
        useArray(new BigDecimal[][]{}[0][0]);
        useArray(new byte[][]{}[0][0]);
        useArray(new boolean[][]{}[0][0]);
        useArray(new char[][]{}[0][0]);
        useArray(new short[][]{}[0][0]);
    }

    private void test0C() {
        useArray(new int[][][]{}[0][0][0]);
        useArray(new float[][][]{}[0][0][0]);
        useArray(new long[][][]{}[0][0][0]);
        useArray(new double[][][]{}[0][0][0]);
        useArray(new BigDecimal[][][]{}[0][0][0]);
        useArray(new byte[][][]{}[0][0][0]);
        useArray(new boolean[][][]{}[0][0][0]);
        useArray(new char[][][]{}[0][0][0]);
        useArray(new short[][][]{}[0][0][0]);
    }

    private void test1A() {
        useArray(new int[][]{}[0]);
        useArray(new float[][]{}[0]);
        useArray(new long[][]{}[0]);
        useArray(new double[][]{}[0]);
        useArray(new BigDecimal[][]{}[0]);
        useArray(new byte[][]{}[0]);
        useArray(new boolean[][]{}[0]);
        useArray(new char[][]{}[0]);
        useArray(new short[][]{}[0]);
    }

    private void test1B() {
        useArray(new int[][][]{}[0][0]);
        useArray(new float[][][]{}[0][0]);
        useArray(new long[][][]{}[0][0]);
        useArray(new double[][][]{}[0][0]);
        useArray(new BigDecimal[][][]{}[0][0]);
        useArray(new byte[][][]{}[0][0]);
        useArray(new boolean[][][]{}[0][0]);
        useArray(new char[][][]{}[0][0]);
        useArray(new short[][][]{}[0][0]);
    }

    private void test2A() {
        useArray(new int[][][]{}[0]);
        useArray(new float[][][]{}[0]);
        useArray(new long[][][]{}[0]);
        useArray(new double[][][]{}[0]);
        useArray(new BigDecimal[][][]{}[0]);
        useArray(new byte[][][]{}[0]);
        useArray(new boolean[][][]{}[0]);
        useArray(new char[][][]{}[0]);
        useArray(new short[][][]{}[0]);
    }

    private void test3() {
        useArray(new int[1][2][3]);
        useArray(new float[1][2][3]);
        useArray(new long[1][2][3]);
        useArray(new double[1][2][3]);
        useArray(new BigDecimal[1][2][3]);
        useArray(new byte[1][2][3]);
        useArray(new boolean[1][2][3]);
        useArray(new char[1][2][3]);
        useArray(new short[1][2][3]);
    }

    private void test4() {
        useArray(new int[][]{
                {111, 112},
                {221, 200}
        });
    }

    private void useArray(Object obj) {
    }
}
