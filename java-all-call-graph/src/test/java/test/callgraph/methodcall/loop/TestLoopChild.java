package test.callgraph.methodcall.loop;

/**
 * @author adrninistrator
 * @date 2024/11/4
 * @description:
 */
public class TestLoopChild extends TestLoopSuper implements TestLoopInterface {

    @Override
    public void func2(String str1, String str2) {
        TestLoopCallee.test();
    }
}
