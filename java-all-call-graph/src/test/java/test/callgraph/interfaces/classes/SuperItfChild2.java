package test.callgraph.interfaces.classes;

import test.callgraph.interfaces.interfaces.SuperItfItf1;

/**
 * @author adrninistrator
 * @date 2024/7/12
 * @description:
 */
public class SuperItfChild2 extends SuperItfSuper2 implements SuperItfItf1 {
    public void test1() {
        superCommon2();
    }

    @Override
    public void itfCustom() {
        System.setOut(null);
    }
}
