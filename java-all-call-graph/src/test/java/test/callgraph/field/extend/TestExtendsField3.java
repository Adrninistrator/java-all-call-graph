package test.callgraph.field.extend;

import test.callgraph.field.cycle.TestFieldCycle1;
import test.callgraph.field.cycle.TestFieldGenericsCycle1;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2024/10/25
 * @description:
 */
public class TestExtendsField3 {

    private int i3;

    private TestFieldCycle1 testFieldCycle1;

    private List<TestFieldGenericsCycle1> testFieldGenericsCycle1List;

    public int getI3() {
        return i3;
    }

    public void setI3(int i3) {
        this.i3 = i3;
    }

    public TestFieldCycle1 getTestFieldCycle1() {
        return testFieldCycle1;
    }

    public void setTestFieldCycle1(TestFieldCycle1 testFieldCycle1) {
        this.testFieldCycle1 = testFieldCycle1;
    }

    public List<TestFieldGenericsCycle1> getTestFieldGenericsCycle1List() {
        return testFieldGenericsCycle1List;
    }

    public void setTestFieldGenericsCycle1List(List<TestFieldGenericsCycle1> testFieldGenericsCycle1List) {
        this.testFieldGenericsCycle1List = testFieldGenericsCycle1List;
    }
}
