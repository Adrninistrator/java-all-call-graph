package test.callgraph.field.cycle;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2024/10/25
 * @description:
 */
public class TestUseFieldGenericsCycle1 {

    private List<TestFieldGenericsCycle1> testFieldGenericsCycle1List;

    public List<TestFieldGenericsCycle1> getTestFieldGenericsCycle1List() {
        return testFieldGenericsCycle1List;
    }

    public void setTestFieldGenericsCycle1List(List<TestFieldGenericsCycle1> testFieldGenericsCycle1List) {
        this.testFieldGenericsCycle1List = testFieldGenericsCycle1List;
    }
}
