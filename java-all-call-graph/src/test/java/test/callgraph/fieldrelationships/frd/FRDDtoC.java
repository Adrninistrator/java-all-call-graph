package test.callgraph.fieldrelationships.frd;

import com.fasterxml.jackson.annotation.JsonProperty;

import java.math.BigDecimal;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/8/5
 * @description:
 */
public class FRDDtoC {

    private String string1;

    private BigDecimal bigDecimal1;

    private int i1;

    private int i2;

    private int i3;

    @JsonProperty("_frdDtoA")
    private FRDDtoA frdDtoA;

    @JsonProperty("_frdDtoB")
    private FRDDtoB frdDtoB;

    private List<FRDDtoA> frdDtoAList;

    public String getString1() {
        return string1;
    }

    public void setString1(String string1) {
        this.string1 = string1;
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

    public FRDDtoB getFrdDtoB() {
        return frdDtoB;
    }

    public void setFrdDtoB(FRDDtoB frdDtoB) {
        this.frdDtoB = frdDtoB;
    }

    public List<FRDDtoA> getFrdDtoAList() {
        return frdDtoAList;
    }

    public void setFrdDtoAList(List<FRDDtoA> frdDtoAList) {
        this.frdDtoAList = frdDtoAList;
    }
}
