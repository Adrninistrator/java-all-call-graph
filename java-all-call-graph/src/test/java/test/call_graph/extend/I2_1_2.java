package test.call_graph.extend;

/**
 * @author adrninistrator
 * @date 2021/6/27
 * @description:
 */

public class I2_1_2 extends A2_1 {
    @Override
    public void f1() {
    }

    @Override
    public void f2() {
        fi2_1_2();
    }

    protected void fi2_1_2() {
        fa1_1();
    }
}
