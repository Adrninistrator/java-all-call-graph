package test.callgraph.field.cycle;

import java.math.BigDecimal;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2024/10/25
 * @description:
 */
public class TestFieldGenericsCycle3 {

    private int i1;

    private String str1;

    private BigDecimal bigDecimal1;

    private List<TestFieldGenericsCycle2> testFieldGenericsCycle2ListA;
    private List<TestFieldGenericsCycle2> testFieldGenericsCycle2ListB;

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

    public List<TestFieldGenericsCycle2> getTestFieldGenericsCycle2ListA() {
        return testFieldGenericsCycle2ListA;
    }

    public void setTestFieldGenericsCycle2ListA(List<TestFieldGenericsCycle2> testFieldGenericsCycle2ListA) {
        this.testFieldGenericsCycle2ListA = testFieldGenericsCycle2ListA;
    }

    public List<TestFieldGenericsCycle2> getTestFieldGenericsCycle2ListB() {
        return testFieldGenericsCycle2ListB;
    }

    public void setTestFieldGenericsCycle2ListB(List<TestFieldGenericsCycle2> testFieldGenericsCycle2ListB) {
        this.testFieldGenericsCycle2ListB = testFieldGenericsCycle2ListB;
    }
}
