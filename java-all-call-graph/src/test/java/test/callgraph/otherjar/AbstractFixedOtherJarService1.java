package test.callgraph.otherjar;

/**
 * @author adrninistrator
 * @date 2023/3/24
 * @description:
 */
public abstract class AbstractFixedOtherJarService1 {
    public String invoke() {
        return execute();
    }

    protected abstract String execute();
}
