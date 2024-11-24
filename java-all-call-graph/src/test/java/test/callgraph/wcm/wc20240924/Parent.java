package test.callgraph.wcm.wc20240924;

/**
 * @author adrninistrator
 * @date 2024/9/25
 * @description:
 */
public abstract class Parent {
    public void f1() {
        f2();
    }

    public abstract void f2();
}
