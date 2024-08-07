package test.callgraph.manualaddmethodcall.fixed;

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
