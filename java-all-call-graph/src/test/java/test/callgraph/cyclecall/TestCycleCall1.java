package test.callgraph.cyclecall;

/**
 * @author adrninistrator
 * @date 2022/6/25
 * @description:
 */
public class TestCycleCall1 {
    public void test1() {
        test2();
    }

    public void test2() {
        test3();
    }

    public void test3() {
        test1();
        test4();
    }

    public void test4() {
        test2();
    }

    public void test5() {
        System.out.println("abc");
        while (System.currentTimeMillis() % 10 != 7) {
            test5();
        }
    }
}
