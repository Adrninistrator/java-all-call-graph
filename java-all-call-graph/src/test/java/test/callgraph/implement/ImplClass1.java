package test.callgraph.implement;

/**
 * @author adrninistrator
 * @date 2021/8/1
 * @description:
 */

public class ImplClass1 implements Interface1 {
    @Override
    public void f1() {
        System.out.println("");
    }

    @Override
    public void f2() {
        System.getProperty("");
    }
}
