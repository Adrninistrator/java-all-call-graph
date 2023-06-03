package test.call_graph.cycle_call;

/**
 * @author adrninistrator
 * @date 2023/5/18
 * @description:
 */
public class TestArrayCycle {

    public void testReflectionArrayCycle() {
        final Object[] objects = new Object[1];
        objects[0] = objects;
        test(objects);
    }

    public void testReflectionArrayCycleLevel2() {
        final Object[] objects = new Object[1];
        final Object[] objectsLevel2 = new Object[1];
        objects[0] = objectsLevel2;
        objectsLevel2[0] = objects;
        test(objects);
    }

    private void test(Object[] objects) {
    }
}
