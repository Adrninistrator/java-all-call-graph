package test.callgraph.extendcomplex;

/**
 * @author adrninistrator
 * @date 2022/9/6
 * @description:
 */
public abstract class AbstractSuperClassA {
    protected void entryA() {
        System.setIn(null);
        runA();
        runB();
        runC();
    }

    protected void entryB() {
        runD();
    }

    protected abstract void runA();

    protected abstract void runB();

    protected abstract void runC();

    protected abstract void runD();
}
