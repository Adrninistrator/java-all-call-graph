package test.call_graph.future;

import java.util.concurrent.Callable;

public class CallableImpl implements Callable {

    @Override
    public Object call() throws Exception {
        return System.currentTimeMillis();
    }
}
