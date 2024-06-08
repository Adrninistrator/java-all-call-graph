package test.callgraph.thread.threadpool;

import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

/**
 * @author adrninistrator
 * @date 2023/3/2
 * @description:
 */
public class TestThreadPool {
    public void test() {
        ThreadPoolExecutor threadPoolExecutor = new ThreadPoolExecutor(1, 1, 10L, TimeUnit.SECONDS,
                new LinkedBlockingQueue<>(10));
        threadPoolExecutor.execute(() -> {
            System.out.println("aaa");
        });
    }
}
