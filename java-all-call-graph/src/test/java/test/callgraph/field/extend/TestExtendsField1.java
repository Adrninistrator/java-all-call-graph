package test.callgraph.field.extend;

import java.math.BigDecimal;

/**
 * @author adrninistrator
 * @date 2024/10/25
 * @description:
 */
public class TestExtendsField1 extends TestExtendsField2 {

    private int i1;

    private String str1;

    private BigDecimal bigDecimal1;

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
}
