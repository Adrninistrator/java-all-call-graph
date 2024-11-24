package test.callgraph.field.cycle;

import java.math.BigDecimal;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2024/10/25
 * @description:
 */
public class TestFieldGenericsCycle1 {

    private int i1;

    private String str1;

    private BigDecimal bigDecimal1;

    private List<TestFieldGenericsCycle3> testFieldGenericsCycle3ListA;
    private List<TestFieldGenericsCycle3> testFieldGenericsCycle3ListB;

    private List<String[]> stringsList;

    private List<byte[]> bytesList;

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

    public List<TestFieldGenericsCycle3> getTestFieldGenericsCycle3ListA() {
        return testFieldGenericsCycle3ListA;
    }

    public void setTestFieldGenericsCycle3ListA(List<TestFieldGenericsCycle3> testFieldGenericsCycle3ListA) {
        this.testFieldGenericsCycle3ListA = testFieldGenericsCycle3ListA;
    }

    public List<TestFieldGenericsCycle3> getTestFieldGenericsCycle3ListB() {
        return testFieldGenericsCycle3ListB;
    }

    public void setTestFieldGenericsCycle3ListB(List<TestFieldGenericsCycle3> testFieldGenericsCycle3ListB) {
        this.testFieldGenericsCycle3ListB = testFieldGenericsCycle3ListB;
    }

    public List<String[]> getStringsList() {
        return stringsList;
    }

    public void setStringsList(List<String[]> stringsList) {
        this.stringsList = stringsList;
    }

    public List<byte[]> getBytesList() {
        return bytesList;
    }

    public void setBytesList(List<byte[]> bytesList) {
        this.bytesList = bytesList;
    }
}
