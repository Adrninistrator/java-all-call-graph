package test.callgraph.interfacesgeneric.interfaces;

import test.callgraph.extendcomplex.ChildClassA1;
import test.callgraph.extendcomplex.ChildClassB2;

/**
 * @author adrninistrator
 * @date 2023/7/9
 * @description:
 */
public interface GenericInterfaceChild1 extends GenericInterfaceSuper1<ChildClassA1, ChildClassB2> {
    @Override
    void test(ChildClassA1 t1, ChildClassB2 t2, String str, int i);
}
