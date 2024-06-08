package test.callgraph.implement;

/**
 * @author adrninistrator
 * @date 2021/8/2
 * @description:
 */

public class ChildClass1 extends AbstractClass1 {
    @Override
    public void f1() {
        System.getProperty("");
    }

    @Override
    public void f2() {
        System.exit(1);
    }
}
