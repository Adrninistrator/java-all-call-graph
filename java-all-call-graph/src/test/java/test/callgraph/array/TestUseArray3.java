package test.callgraph.array;

/**
 * @author adrninistrator
 * @date 2025/5/22
 * @description:
 */
public class TestUseArray3 {

    public void test1() {
        boolean flag = System.currentTimeMillis() % 7 == 1;
        Boolean[] array = flag ? new Boolean[]{} : null;
        for (Boolean b : array) {
            System.out.println(b);
        }
    }

    public void test2() {
        boolean flag = System.currentTimeMillis() % 7 == 1;
        Boolean[] array = flag ? null : new Boolean[]{};
        for (Boolean b : array) {
            System.out.println(b);
        }
    }
}
