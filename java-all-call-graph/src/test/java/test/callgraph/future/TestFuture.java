package test.callgraph.future;

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

    public String test2() throws ExecutionException, InterruptedException {
        FutureTaskChild futureTaskChild = new FutureTaskChild(new CallableImpl());
        futureTaskChild.get();
        new CallableImpl();
        return "";
    }

    public boolean test3() throws ExecutionException, InterruptedException {
        FutureTaskChild futureTaskChild = new FutureTaskChild(new Callable<String>() {
            @Override
            public String call() throws Exception {
                return System.getProperty("");
            }
        });
        futureTaskChild.get();
        return true;
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

    public boolean test5() throws ExecutionException, InterruptedException {
        FutureTaskChild futureTaskChild = new FutureTaskChild(new Runnable() {
            @Override
            public void run() {
                System.getProperty("");
            }
        }, null);
        futureTaskChild.get();
        return false;
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
