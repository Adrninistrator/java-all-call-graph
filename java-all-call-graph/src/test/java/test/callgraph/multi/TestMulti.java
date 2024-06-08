package test.callgraph.multi;

import test.callgraph.extend.A1_1;
import test.callgraph.implement.AbstractClass1;
import test.callgraph.implement.Interface1;

/**
 * @author adrninistrator
 * @date 2021/8/2
 * @description:
 */

public class TestMulti {

    private Interface1 interface1;

    private A1_1 a1_1;

    private AbstractClass1 abstractClass1;

    public void test1() {
        interface1.f1();
        interface1.f2();

        a1_1.f1();
        a1_1.f2();
    }

    public void test2() {
        abstractClass1.f1();
        abstractClass1.f2();
    }
}
