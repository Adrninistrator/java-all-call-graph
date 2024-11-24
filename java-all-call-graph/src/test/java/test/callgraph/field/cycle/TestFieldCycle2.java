package test.callgraph.field.cycle;

import java.math.BigDecimal;

/**
 * @author adrninistrator
 * @date 2024/10/25
 * @description:
 */
public class TestFieldCycle2 {

    private int i1;

    private String str1;

    private BigDecimal bigDecimal1;

    private TestFieldCycle1 testFieldCycle1A;
    private TestFieldCycle1 testFieldCycle1B;

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

    public TestFieldCycle1 getTestFieldCycle1A() {
        return testFieldCycle1A;
    }

    public void setTestFieldCycle1A(TestFieldCycle1 testFieldCycle1A) {
        this.testFieldCycle1A = testFieldCycle1A;
    }

    public TestFieldCycle1 getTestFieldCycle1B() {
        return testFieldCycle1B;
    }

    public void setTestFieldCycle1B(TestFieldCycle1 testFieldCycle1B) {
        this.testFieldCycle1B = testFieldCycle1B;
    }
}
