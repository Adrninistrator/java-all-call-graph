package test.callgraph.field.cycle;

import java.math.BigDecimal;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2024/10/25
 * @description:
 */
public class TestFieldGenericsCycle2 {

    private int i1;

    private String str1;

    private BigDecimal bigDecimal1;

    private List<TestFieldGenericsCycle1> testFieldGenericsCycle1ListA;
    private List<TestFieldGenericsCycle1> testFieldGenericsCycle1ListB;

    public int getI1() {
        return i1;
    }

    public void setI1(int i1) {
        this.i1 = i1;
    }

    public String getStr1() {
        return str1;
    }

    public void setStr1(String str1) {
        this.str1 = str1;
    }

    public BigDecimal getBigDecimal1() {
        return bigDecimal1;
    }

    public void setBigDecimal1(BigDecimal bigDecimal1) {
        this.bigDecimal1 = bigDecimal1;
    }

    public List<TestFieldGenericsCycle1> getTestFieldGenericsCycle1ListA() {
        return testFieldGenericsCycle1ListA;
    }

    public void setTestFieldGenericsCycle1ListA(List<TestFieldGenericsCycle1> testFieldGenericsCycle1ListA) {
        this.testFieldGenericsCycle1ListA = testFieldGenericsCycle1ListA;
    }

    public List<TestFieldGenericsCycle1> getTestFieldGenericsCycle1ListB() {
        return testFieldGenericsCycle1ListB;
    }

    public void setTestFieldGenericsCycle1ListB(List<TestFieldGenericsCycle1> testFieldGenericsCycle1ListB) {
        this.testFieldGenericsCycle1ListB = testFieldGenericsCycle1ListB;
    }
}
