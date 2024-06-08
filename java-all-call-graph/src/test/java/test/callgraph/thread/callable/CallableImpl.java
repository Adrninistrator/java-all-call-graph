package test.callgraph.thread.callable;

import java.util.concurrent.Callable;

/**
 * @author adrninistrator
 * @date 2022/11/20
 * @description:
 */
public class CallableImpl implements Callable<String> {
    @Override
    public String call() throws Exception {
        return String.valueOf(System.currentTimeMillis());
    }
}
