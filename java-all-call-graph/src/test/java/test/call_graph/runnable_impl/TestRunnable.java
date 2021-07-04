package test.call_graph.runnable_impl;

/**
 * @author adrninistrator
 * @date 2021/6/28
 * @description:
 */

public class TestRunnable {

    private void f1() {
        new RunnableImpl1().run();
    }

    private void f2() {
        new Thread(new RunnableImpl1()).start();
    }

    private void f3() {
        new Thread(new RunnableImpl1()).start();
    }

    private void f4() {
        new Thread(() -> System.out.println("1")).start();
    }
}
