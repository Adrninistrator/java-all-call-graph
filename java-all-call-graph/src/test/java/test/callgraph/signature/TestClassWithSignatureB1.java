package test.callgraph.signature;

import test.callgraph.fieldrelationships.fra.FRAClass1;
import test.callgraph.fieldrelationships.fra.FRAClass2;
import test.callgraph.fieldrelationships.fra.FRAClass3;
import test.callgraph.fieldrelationships.fra.FRAClass4;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2025/1/5
 * @description:
 */
public class TestClassWithSignatureB1 extends TestAbstractClassWithSignatureB<FRAClass1, FRAClass2, FRAClass3, FRAClass4> {
    @Override
    public void testB1(FRAClass1 fraClass1, FRAClass2 fraClass2, FRAClass3 fraClass3, FRAClass4 fraClass4) {

    }

    @Override
    public void test() {

    }

    @Override
    public FRAClass3 test2(FRAClass2 fraClass2) {
        return null;
    }

    @Override
    public FRAClass3 test3(List<String> stringList) {
        return null;
    }
}
