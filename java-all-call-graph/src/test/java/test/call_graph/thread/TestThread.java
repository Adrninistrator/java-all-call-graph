package test.call_graph.thread;

import java.security.SecureRandom;

public class TestThread {

    public void test1() {
        new ThreadChild().start();
    }

    public void test2() {
        new ThreadChild().run();
    }

    public void test3() {
        new ThreadInner().start();
    }

    public void test4() {
        new Thread() {
            @Override
            public void run() {
                System.getProperty("");
            }
        }.start();
    }

    class ThreadInner extends Thread {

        @Override
        public void run() {
            new SecureRandom().nextInt();
        }
    }
}
