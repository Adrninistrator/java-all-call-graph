package test.callgraph.extendcomplex;

/**
 * @author adrninistrator
 * @date 2022/9/6
 * @description:
 */
public abstract class AbstractSuperClassB extends AbstractSuperClassA {
    @Override
    protected void entryA() {
        System.currentTimeMillis();
        runA();
        runB();
        runC();
        System.currentTimeMillis();
    }

    @Override
    protected void runA() {
        System.getProperty("");
        runB();
    }

    @Override
    protected void runB() {
        System.getProperty("");
    }

    @Override
    protected void runC() {
        System.getProperty("");
    }

    @Override
    protected void runD() {
        System.getProperty("");
    }
}
