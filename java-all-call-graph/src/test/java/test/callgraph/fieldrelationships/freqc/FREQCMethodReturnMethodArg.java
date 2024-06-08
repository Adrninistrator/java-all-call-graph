package test.callgraph.fieldrelationships.freqc;

import test.callgraph.fieldrelationships.freqc.dto.FREQCDtoA;
import test.callgraph.fieldrelationships.freqc.dto.FREQCDtoB;

/**
 * @author adrninistrator
 * @date 2024/2/19
 * @description:
 */
public class FREQCMethodReturnMethodArg {

    public static String returnArg1(String str1) {
        return str1.trim();
    }

    public static String returnArg2(String str1) {
        return str1.trim().trim();
    }

    public static String returnArg3(String str1) {
        if (System.getProperty("") != null) {
            return str1;
        }
        return str1.trim().trim();
    }

    public static void test1() {
        FREQCDtoA freqcDtoA = new FREQCDtoA();
        FREQCDtoB freqcDtoB = new FREQCDtoB();
        String str1 = returnArg1(freqcDtoA.getString1());
        freqcDtoB.setString1(str1);
    }

    public static void test2() {
        FREQCDtoA freqcDtoA = new FREQCDtoA();
        FREQCDtoB freqcDtoB = new FREQCDtoB();
        String str2 = returnArg2(freqcDtoA.getString2());
        freqcDtoB.setString1(str2);
    }
}
