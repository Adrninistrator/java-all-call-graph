package test.callgraph.signature.child;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2025/1/6
 * @description:
 */
public class TestChildClassWithSignature2 implements TestChildInterfaceWithSignature2<Integer, Long, Double, Float> {
    @Override
    public void fC2(Float aFloat, Double aDouble, Long aLong, Integer integer) {

    }

    @Override
    public void testC1(Float aFloat, Double aDouble, Long aLong, Integer integer) {

    }

    @Override
    public void test() {

    }

    @Override
    public Integer test2(Long aLong) {
        return 0;
    }

    @Override
    public Integer test3(List<String> stringList) {
        return 0;
    }
}
