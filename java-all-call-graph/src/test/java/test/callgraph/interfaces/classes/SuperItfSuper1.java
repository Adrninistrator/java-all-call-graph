package test.callgraph.interfaces.classes;

import test.callgraph.interfaces.interfaces.SuperItfItf1;

/**
 * @author adrninistrator
 * @date 2024/7/12
 * @description:
 */
public abstract class SuperItfSuper1 {

    private SuperItfItf1 getSuperItfItf1() {
        return (SuperItfItf1) this;
    }

    public void superCommon1() {
        getSuperItfItf1().itfCommon();
    }
}
