package test.callgraph.interfaces.classes;

import test.callgraph.interfaces.interfaces.SuperItfItf1;

/**
 * @author adrninistrator
 * @date 2024/7/12
 * @description:
 */
public class SuperItfImpl2 implements SuperItfItf1 {

    @Override
    public void itfCustom() {
        System.setIn(null);
    }
}
