package test.call_graph.variable_argument;

/**
 * @author adrninistrator
 * @date 2022/5/8
 * @description:
 */
public class TestVAArray1 {
    private void test1() {
        String[] array1 = new String[]{};
        String[] array2 = new String[]{"1"};
    }

    private void test2() {
        String[][] array1 = new String[][]{};
        String[][] array2 = new String[][]{new String[]{"1", "2"}, new String[]{"a", "b", "c"}};
    }
}
