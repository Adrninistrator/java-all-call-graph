package test.callgraph.manualaddmethodcall.fixed;

import test.callgraph.otherjar.AbstractFixedOtherJarService1;

/**
 * @author adrninistrator
 * @date 2023/3/24
 * @description:
 */
public class FixedOtherJarService1a extends AbstractFixedOtherJarService1 {
    @Override
    protected String execute() {
        return System.getProperty("aaa");
    }
}
