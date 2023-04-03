package test.call_graph.manual_add_method_call.fixed;

/**
 * @author adrninistrator
 * @date 2022/4/15
 * @description:
 */
public abstract class AbstractFixedService1 implements Runnable {
    public String invoke() {
        return execute();
    }

    protected abstract String execute();

    @Override
    public void run() {
    }
}
