package test.call_graph.runnable_impl;

import java.security.SecureRandom;

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
        new Thread(new RunnableImpl1()).run();
    }

    private void f4() {
        new Thread(() -> {
            int a = 1;
            System.getProperty("");
            int b = 2;
            System.out.println("1");
            int c = 3;
            new SecureRandom().nextInt();
        }).start();

        new Thread(() -> {
            int a = 1;
            System.out.println("2");
        }).start();
    }
}
