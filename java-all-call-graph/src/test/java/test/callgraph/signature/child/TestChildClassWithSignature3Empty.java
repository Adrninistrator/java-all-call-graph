package test.callgraph.signature.child;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2025/1/6
 * @description:
 */
public class TestChildClassWithSignature3Empty implements TestChildInterfaceWithSignature1 {
    @Override
    public void testC1(Object object, Object object2, Object object3, Object object4) {

    }

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
