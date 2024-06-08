package test.callgraph.fieldrelationships.freqc;

import test.callgraph.fieldrelationships.freqc.dto.FREQCDtoB;

/**
 * @author adrninistrator
 * @date 2024/2/19
 * @description:
 */
public class FREQCMixed {

    public static void useArg1(String str1) {
        FREQCDtoB freqcDtoB = new FREQCDtoB();
        freqcDtoB.setString1(str1.trim());
        freqcDtoB.setString2(str1.trim().trim());
    }

    public static String returnString1() {
        FREQCDtoB freqcDtoB = new FREQCDtoB();
        return freqcDtoB.getString1().trim().trim();
    }

    public static String returnArg1(String str1) {
        return str1.trim().trim();
    }

    public static void testUseArgMCReturn1() {
        useArg1(returnString1());
    }

    public static void testUseArgMCReturn2() {
        useArg1(returnString1().trim());
    }

    public static void testUseArgReturnArg1() {
        String str1 = returnString1();
        String str2 = returnArg1(str1);
        useArg1(str2);
    }

    public static void testUseArgReturnArg2() {
        String str1 = returnString1().trim();
        String str2 = returnArg1(str1).trim();
        useArg1(str2);
    }
}
