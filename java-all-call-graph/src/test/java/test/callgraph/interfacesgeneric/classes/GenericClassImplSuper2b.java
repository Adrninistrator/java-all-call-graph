package test.callgraph.interfacesgeneric.classes;

import test.callgraph.innerclass.TestChildInInnerData;
import test.callgraph.innerclass.TestInClass;

/**
 * @author adrninistrator
 * @date 2023/8/8
 * @description:
 */
public class GenericClassImplSuper2b extends GenericAbstractSuper2<TestInClass.TestInInnerData.TestInInnerData2, TestChildInInnerData> {
    @Override
    public void test(TestInClass.TestInInnerData.TestInInnerData2 testInInnerData2, TestChildInInnerData testChildInInnerData, String str, int i) {
    }
}
