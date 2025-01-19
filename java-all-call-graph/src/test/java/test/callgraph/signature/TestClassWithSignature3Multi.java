package test.callgraph.signature;

import test.callgraph.field.TestField1;
import test.callgraph.signature.child.TestChildInterfaceWithSignature1;

import java.math.BigDecimal;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2025/1/6
 * @description:
 */
public class TestClassWithSignature3Multi extends TestField1 implements TestInterfaceWithSignature2<Long, Integer>,
        TestChildInterfaceWithSignature1<String, BigDecimal, Character, Double> {
    @Override
    public void testC1(String s, BigDecimal bigDecimal, Character character, Double aDouble) {

    }

    @Override
    public Double test2(Character character) {
        return 0.0;
    }

    @Override
    public Double test3(List<String> stringList) {
        return 0.0;
    }

    @Override
    public void t(Long aLong, Integer integer) {

    }
}
