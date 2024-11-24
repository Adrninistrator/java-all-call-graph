package test.callgraph.field.cycle;

import java.math.BigDecimal;

/**
 * @author adrninistrator
 * @date 2024/10/25
 * @description:
 */
public class TestFieldCycle1 {

    private int i1;

    private String str1;

    private BigDecimal bigDecimal1;

    private TestFieldCycle3 testFieldCycle3A;
    private TestFieldCycle3 testFieldCycle3B;

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

    public TestFieldCycle3 getTestFieldCycle3A() {
        return testFieldCycle3A;
    }

    public void setTestFieldCycle3A(TestFieldCycle3 testFieldCycle3A) {
        this.testFieldCycle3A = testFieldCycle3A;
    }

    public TestFieldCycle3 getTestFieldCycle3B() {
        return testFieldCycle3B;
    }

    public void setTestFieldCycle3B(TestFieldCycle3 testFieldCycle3B) {
        this.testFieldCycle3B = testFieldCycle3B;
    }
}
