package test.callgraph.methodcall.loop;

/**
 * @author adrninistrator
 * @date 2024/11/4
 * @description:
 */
public class TestLoopCaller {

    public void test() {
        TestLoopInterface testLoopInterface = null;
        testLoopInterface.func1(null);
        testLoopInterface.func2(null, null);
        TestLoopSuper testLoopSuper = null;
        testLoopSuper.func1(null);
        testLoopSuper.func2(null, null);
        TestLoopChild testLoopChild = null;
        testLoopChild.func1(null);
        testLoopChild.func2(null, null);
    }
}
