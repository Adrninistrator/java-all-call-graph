package test.call_graph.future;

import java.util.concurrent.*;

public class FutureTaskChild extends FutureTask {

    public FutureTaskChild(Callable callable) {
        super(callable);
    }

    public FutureTaskChild(Runnable runnable, Object result) {
        super(runnable, result);
    }
}
