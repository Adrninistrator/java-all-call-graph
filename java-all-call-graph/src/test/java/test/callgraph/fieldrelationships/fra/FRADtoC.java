package test.callgraph.fieldrelationships.fra;

import com.fasterxml.jackson.annotation.JsonProperty;
import org.apache.commons.lang3.StringUtils;

/**
 * @author adrninistrator
 * @date 2023/7/22
 * @description:
 */
public class FRADtoC {
    private int i1;

    private Integer integer1;

    private Long l1;

    private String str1;

    @JsonProperty("str2")
    private String str2OtherName;

    private String str3;

    private String str4;

    private String str5;

    public int getI1() {
        return i1;
    }

    public void setI1(int i1) {
        this.i1 = i1;
    }

    public Integer getInteger1() {
        return (integer1 == null || integer1 == 0 || integer1 > 100) ? 20 : integer1;
    }

    public void setInteger1(Integer integer1) {
        this.integer1 = integer1;
    }

    public Long getL1() {
        return l1;
    }

    public void setL1(Long l1) {
        this.l1 = l1;
    }

    public String getStr1() {
        return str1;
    }

    public void setStr1(String str1) {
        this.str1 = str1;
    }

    public String getStr2OtherName() {
        return str2OtherName;
    }

    public void setStr2OtherName(String str2OtherName) {
        this.str2OtherName = str2OtherName;
    }

    public String getStr3() {
        return str3 == null ? null : str3.trim();
    }

    public void setStr3(String str3) {
        this.str3 = (str3 == null ? null : str3.trim());
    }

    public String getStr4() {
        return str4 == null ? "" : str4;
    }

    public void setStr4(String str4) {
        this.str4 = (str4 == null ? "" : str4);
    }

    public String getStr5() {
        return StringUtils.substring(str5, 0, 100);
    }

    public void setStr5(String str5) {
        this.str5 = StringUtils.substring(str5, 0, 100);
    }
}
