package test.callgraph.extendcomplex2;

/**
 * @author adrninistrator
 * @date 2023/6/1
 * @description:
 */
public abstract class EC2AService {
    public String exec() {
        get();
        return "";
    }

    protected abstract void get();
}
