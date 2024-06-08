package test.callgraph.interfacesgeneric.classes;

import test.callgraph.extendcomplex.ChildClassA2;
import test.callgraph.extendcomplex.ChildClassB1;
import test.callgraph.interfacesgeneric.interfaces.GenericInterfaceSuper1;

/**
 * @author adrninistrator
 * @date 2023/7/9
 * @description:
 */
public class GenericClassImplSuper1 implements GenericInterfaceSuper1<ChildClassA2, ChildClassB1> {
    @Override
    public void test(ChildClassA2 t1, ChildClassB1 t2, String str, int i) {
        System.out.println("aaa");
    }
}
