package test.callgraph.signature;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2025/1/
 * @description:
 */
public class TestClassWithSignature4Empty implements TestInterfaceWithSignature1 {
    @Override
    public void test() {

    }

    @Override
    public Object test2(Object object) {
        return null;
    }

    @Override
    public Object test3(List list) {
        return null;
    }
}
