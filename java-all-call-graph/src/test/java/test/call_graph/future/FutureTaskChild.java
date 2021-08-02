package test.call_graph.future;

import java.util.concurrent.*;

/**
 * @author adrninistrator
 * @date 2021/7/28
 * @description:
 */

public class FutureTaskChild extends FutureTask {

    public FutureTaskChild(Callable callable) {
        super(callable);
    }

    public FutureTaskChild(Runnable runnable, Object result) {
        super(runnable, result);
    }
}
