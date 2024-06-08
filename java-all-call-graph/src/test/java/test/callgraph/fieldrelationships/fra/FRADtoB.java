package test.callgraph.fieldrelationships.fra;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * @author adrninistrator
 * @date 2023/6/9
 * @description:
 */
public class FRADtoB {
    public int intB1;

    public Long longB1;

    @JsonProperty("str_field1")
    public String strFieldB1;

    public String str2;

    public int getIntB1() {
        return intB1;
    }

    public void setIntB1(int intB1) {
        this.intB1 = intB1;
    }

    public Long getLongB1() {
        return longB1;
    }

    public void setLongB1(Long longB1) {
        this.longB1 = longB1;
    }

    public String getStrFieldB1() {
        return strFieldB1;
    }

    public void setStrFieldB1(String strFieldB1) {
        this.strFieldB1 = strFieldB1;
    }

    public String getStr2() {
        return str2;
    }

    public void setStr2(String str2) {
        this.str2 = str2;
    }
}
