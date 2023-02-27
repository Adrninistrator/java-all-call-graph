package test.call_graph.thread.callable;

import java.util.concurrent.Callable;
import java.util.concurrent.FutureTask;

/**
 * @author adrninistrator
 * @date 2022/11/20
 * @description:
 */
public class TestCallable {
    public void test1() {
        Callable<String> callable = new Callable<String>() {
            @Override
            public String call() throws Exception {
                return System.getProperty("");
            }
        };
        FutureTask<String> futureTask = new FutureTask<>(callable);
        try {
            futureTask.get();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void test2() {
        FutureTask<String> futureTask = new FutureTask<>(new CallableImpl());
        try {
            futureTask.get();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void test3() {
        FutureTask<String> futureTask = new FutureTask<>(new CallableImpl());
        try {
            futureTask.get();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
