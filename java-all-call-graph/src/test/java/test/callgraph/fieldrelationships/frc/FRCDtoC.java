package test.callgraph.fieldrelationships.frc;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * @author adrninistrator
 * @date 2023/7/25
 * @description:
 */
public class FRCDtoC extends FRCDtoB {

    @JsonProperty("str_field1")
    private String strField1;

    private String strField2;

    public String getStrField1() {
        return strField1;
    }

    public void setStrField1(String strField1) {
        this.strField1 = strField1;
    }

    public String getStrField2() {
        return strField2;
    }

    public void setStrField2(String strField2) {
        this.strField2 = strField2;
    }
}
