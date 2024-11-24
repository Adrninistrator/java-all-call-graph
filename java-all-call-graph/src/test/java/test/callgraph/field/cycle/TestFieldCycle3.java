package test.callgraph.field.cycle;

import java.math.BigDecimal;

/**
 * @author adrninistrator
 * @date 2024/10/25
 * @description:
 */
public class TestFieldCycle3 {

    private int i1;

    private String str1;

    private BigDecimal bigDecimal1;

    private TestFieldCycle2 testFieldCycle2A;
    private TestFieldCycle2 testFieldCycle2B;

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

    public TestFieldCycle2 getTestFieldCycle2A() {
        return testFieldCycle2A;
    }

    public void setTestFieldCycle2A(TestFieldCycle2 testFieldCycle2A) {
        this.testFieldCycle2A = testFieldCycle2A;
    }

    public TestFieldCycle2 getTestFieldCycle2B() {
        return testFieldCycle2B;
    }

    public void setTestFieldCycle2B(TestFieldCycle2 testFieldCycle2B) {
        this.testFieldCycle2B = testFieldCycle2B;
    }
}
