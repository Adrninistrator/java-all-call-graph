package test.callgraph.signature;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2022/12/7
 * @description:
 */
public class TestClassWithSignature3 extends TestAbstractClassWithSignature<int[], String[]> {
    @Override
    public void test() {

    }

    @Override
    public String[] test2(int[] testArgument1) {
        superMethod1(testArgument1);
        return null;
    }

    @Override
    public String[] test3(List<String> stringList) {
        String[] testArgument2 = new String[]{};
        superMethod2(testArgument2);
        return testArgument2;
    }
}
