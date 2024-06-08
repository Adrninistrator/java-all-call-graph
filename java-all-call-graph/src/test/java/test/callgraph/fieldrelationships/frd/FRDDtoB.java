package test.callgraph.fieldrelationships.frd;

import com.fasterxml.jackson.annotation.JsonProperty;

import java.math.BigDecimal;

/**
 * @author adrninistrator
 * @date 2023/8/4
 * @description:
 */
public class FRDDtoB {

    private String string1;

    private BigDecimal bigDecimal1;

    private int i1;

    private int i2;

    private int i3;

    @JsonProperty("_frdDtoA")
    private FRDDtoA frdDtoA;

    private FRDDtoA frdDtoA1;

    // 不定义i1对应的get方法
//    public String getString1() {
//        return string1;
//    }

    public void setString1(String string1) {
        this.string1 = string1;
    }

    public void setString1(FRDDtoA frdDtoA) {
        this.string1 = frdDtoA.getString1();
    }

    public BigDecimal getBigDecimal1() {
        return bigDecimal1;
    }

    public void setBigDecimal1(BigDecimal bigDecimal1) {
        this.bigDecimal1 = bigDecimal1;
    }

    public int getI1() {
        return i1;
    }

    public void setI1(int i1) {
        this.i1 = i1;
    }

    public int getI2() {
        return i2;
    }

    public void setI2(int i2) {
        this.i2 = i2;
    }

    public int getI3() {
        return i3;
    }

    public void setI3(int i3) {
        this.i3 = i3;
    }

    public FRDDtoA getFrdDtoA() {
        return frdDtoA;
    }

    public void setFrdDtoA(FRDDtoA frdDtoA) {
        this.frdDtoA = frdDtoA;
    }

    public String getFRDDtoAString1() {
        return frdDtoA.getString1();
    }

    public FRDDtoA getFrdDtoA1() {
        return frdDtoA1;
    }

    public void setFrdDtoA1(FRDDtoA frdDtoA1) {
        this.frdDtoA1 = frdDtoA1;
    }
}
