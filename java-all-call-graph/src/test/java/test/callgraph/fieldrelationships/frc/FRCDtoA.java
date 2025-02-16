package test.callgraph.fieldrelationships.frc;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * @author adrninistrator
 * @date 2023/7/25
 * @description:
 */
public class FRCDtoA {

    @JsonProperty("i_field1")
    private int iField1;

    @JsonProperty("l_field1")
    private Long lFieldA;

    private String strData1;

    // 有使用，不要改名
    public void testStrFRCDtoA(String str) {
        System.getenv(str);
    }

    public int getiField1() {
        return iField1;
    }

    public void setiField1(int iField1) {
        this.iField1 = iField1;
    }

    public Long getlFieldA() {
        return lFieldA;
    }

    public void setlFieldA(Long lFieldA) {
        this.lFieldA = lFieldA;
    }

    public String getStrData1() {
        return strData1;
    }

    public void setStrData1(String strData1) {
        this.strData1 = strData1;
    }
}
