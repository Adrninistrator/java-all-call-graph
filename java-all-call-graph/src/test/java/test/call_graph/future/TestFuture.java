package test.call_graph.future;

import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;

/**
 * @author adrninistrator
 * @date 2021/7/28
 * @description:
 */

public class TestFuture {

    public void test1() throws ExecutionException, InterruptedException {
        FutureImpl futureChild = new FutureImpl();
        futureChild.get();
    }

    public void test2() throws ExecutionException, InterruptedException {
        FutureTaskChild futureTaskChild = new FutureTaskChild(new CallableImpl());
        futureTaskChild.get();
        new CallableImpl();
    }

    public void test3() throws ExecutionException, InterruptedException {
        FutureTaskChild futureTaskChild = new FutureTaskChild(new Callable() {
            @Override
            public Object call() throws Exception {
                return System.getProperty("");
            }
        });
        futureTaskChild.get();
    }

    public void test4() throws ExecutionException, InterruptedException {
        int b = 1;

        FutureTaskChild futureTaskChild = new FutureTaskChild(() -> {
            int a = 1;
            System.getProperty("");
            return null;
        });
        futureTaskChild.get();
    }

    public void test5() throws ExecutionException, InterruptedException {
        FutureTaskChild futureTaskChild = new FutureTaskChild(new Runnable() {
            @Override
            public void run() {
                System.getProperty("");
            }
        }, null);
        futureTaskChild.get();
    }

    public void test6() throws ExecutionException, InterruptedException {
        FutureTaskChild futureTaskChild = new FutureTaskChild(new Runnable() {
            @Override
            public void run() {
                System.getProperty("");
            }
        }, null);
        futureTaskChild.get();
    }
}
