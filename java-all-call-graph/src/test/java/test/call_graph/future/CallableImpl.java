package test.call_graph.future;

import java.util.concurrent.Callable;

/**
 * @author adrninistrator
 * @date 2021/7/28
 * @description:
 */

public class CallableImpl implements Callable {

    @Override
    public Object call() throws Exception {
        return System.currentTimeMillis();
    }
}
