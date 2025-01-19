package test.callgraph.signature;

import test.callgraph.field.TestField1;

import java.math.BigDecimal;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2025/1/6
 * @description:
 */
public class TestClassWithSignature2Multi extends TestField1 implements TestInterfaceWithSignature1<String, BigDecimal>, TestInterfaceWithSignature2<Long, Integer> {
    @Override
    public BigDecimal test2(String s) {
        return null;
    }

    @Override
    public BigDecimal test3(List<String> stringList) {
        return null;
    }

    @Override
    public void t(Long aLong, Integer integer) {

    }
}
