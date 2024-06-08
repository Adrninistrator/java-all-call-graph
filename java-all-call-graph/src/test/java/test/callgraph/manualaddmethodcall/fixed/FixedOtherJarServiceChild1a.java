package test.callgraph.manualaddmethodcall.fixed;

/**
 * @author adrninistrator
 * @date 2023/3/24
 * @description:
 */
public class FixedOtherJarServiceChild1a extends AbstractFixedOtherJarServiceChild1 {
    @Override
    protected String execute() {
        return System.getProperty("aaa");
    }
}
