package test.callgraph.fieldrelationships.frc;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * @author adrninistrator
 * @date 2023/7/25
 * @description:
 */
public class FRCDtoB extends FRCDtoA {

    private int iField1;

    @JsonProperty("l_field1")
    private Long lField1;

    private String str1;

    @Override
    public int getiField1() {
        return iField1;
    }

    @Override
    public void setiField1(int iField1) {
        this.iField1 = iField1;
    }

    public Long getlField1() {
        return lField1;
    }

    public void setlField1(Long lField1) {
        this.lField1 = lField1;
    }

    public String getStr1() {
        return str1;
    }

    public void setStr1(String str1) {
        this.str1 = str1;
    }
}
