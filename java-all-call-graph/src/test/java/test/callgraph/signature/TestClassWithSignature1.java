package test.callgraph.signature;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2022/12/7
 * @description:
 */
public class TestClassWithSignature1 implements TestInterfaceWithSignature<String, Integer> {
    @Override
    public void test() {

    }

    @Override
    public Integer test2(String s) {
        return null;
    }

    @Override
    public Integer test3(List<String> stringList) {
        System.out.println("");
        return null;
    }
}
