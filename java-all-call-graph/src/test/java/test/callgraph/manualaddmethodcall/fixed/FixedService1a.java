package test.callgraph.manualaddmethodcall.fixed;

/**
 * @author adrninistrator
 * @date 2022/4/15
 * @description:
 */
public class FixedService1a extends AbstractFixedService1 {
    @Override
    protected String execute() {
        return System.getProperty("");
    }
}
