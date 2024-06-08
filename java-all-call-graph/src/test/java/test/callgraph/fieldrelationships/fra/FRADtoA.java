package test.callgraph.fieldrelationships.fra;

/**
 * @author adrninistrator
 * @date 2023/6/9
 * @description:
 */
public class FRADtoA {
    private static final String TEST_STRING = "abc";

    public int intA1;

    public Long longA1;

    public String strA1;

    public String strA2;

    public String str3NotUsed;

    public static String getTest() {
        return "";
    }

    public int getIntA1() {
        return intA1;
    }

    public void setIntA1(int intA1) {
        this.intA1 = intA1;
    }

    public Long getLongA1() {
        return longA1;
    }

    public void setLongA1(Long longA1) {
        this.longA1 = longA1;
    }

    public void setL1(long a2) {
        this.longA1 = a2;
    }

    public String getStrA1() {
        return strA1;
    }

    public void setStrA1(String strA1) {
        this.strA1 = strA1;
    }

    public String getStrA2() {
        return strA2;
    }

    public void setStrA2(String strA2) {
        this.strA2 = strA2;
    }

    public String getStr3NotUsed() {
        return str3NotUsed;
    }

    public String getStr3NotUsedOther() {
        String a3Temp = str3NotUsed;
        return a3Temp;
    }

    public String getStr3NotUsedMix() {
        if (intA1 == 0) {
            return TEST_STRING;
        }
        return str3NotUsed;
    }

    public void setStr3NotUsed(String str3NotUsed) {
        this.str3NotUsed = str3NotUsed;
    }
}
