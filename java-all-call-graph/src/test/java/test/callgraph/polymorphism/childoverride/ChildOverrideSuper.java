package test.callgraph.polymorphism.childoverride;

/**
 * @author adrninistrator
 * @date 2023/6/1
 * @description:
 */
public abstract class ChildOverrideSuper {
    public String exec() {
        custom();
        return "";
    }

    protected abstract void custom();
}
