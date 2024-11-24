package test.callgraph.methodcall.loop;

/**
 * @author adrninistrator
 * @date 2024/11/4
 * @description:
 */
public abstract class TestLoopSuper implements TestLoopInterface {

    @Override
    public void func1(String str1) {
        func2(str1, null);
    }
}
