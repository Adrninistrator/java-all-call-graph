package test.callgraph.interfacesgeneric.classes;

import test.callgraph.field.TestField1;

/**
 * @author adrninistrator
 * @date 2023/8/8
 * @description:
 */
public class GenericClassImplSuper2d<E1 extends String, E2 extends TestField1> extends GenericAbstractSuper2<E1, E2> {
    @Override
    public void test(E1 e1, E2 e2, String str, int i) {
    }
}
