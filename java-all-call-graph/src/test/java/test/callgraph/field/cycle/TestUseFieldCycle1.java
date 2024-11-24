package test.callgraph.field.cycle;

/**
 * @author adrninistrator
 * @date 2024/10/25
 * @description:
 */
public class TestUseFieldCycle1 {
    private TestFieldCycle1 testFieldCycle1;

    public TestFieldCycle1 getTestFieldCycle1() {
        return testFieldCycle1;
    }

    public void setTestFieldCycle1(TestFieldCycle1 testFieldCycle1) {
        this.testFieldCycle1 = testFieldCycle1;
    }
}
