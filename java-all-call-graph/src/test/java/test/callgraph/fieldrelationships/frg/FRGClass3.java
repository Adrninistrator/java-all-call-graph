package test.callgraph.fieldrelationships.frg;

import java.text.SimpleDateFormat;

/**
 * @author adrninistrator
 * @date 2024/1/16
 * @description:
 */
public class FRGClass3 {

    public static String transferTimeNotEq(String value, String str1) {
        return new SimpleDateFormat(str1).format(value);
    }

    public static String transferValue(String value, String other) {
        System.out.println(other);
        return transferValue2(value.trim());
    }

    public static String transferValue2(String value) {
        return value.trim();
    }

    public static String transferValueNoTrim(String value, String other) {
        System.out.println(other);
        return transferValue2NoTrim(value);
    }

    public static String transferValue2NoTrim(String value) {
        return value;
    }
}
