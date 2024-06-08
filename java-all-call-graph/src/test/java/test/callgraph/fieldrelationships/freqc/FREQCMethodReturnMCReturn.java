package test.callgraph.fieldrelationships.freqc;

import test.callgraph.fieldrelationships.freqc.dto.FREQCDtoA;
import test.callgraph.fieldrelationships.freqc.dto.FREQCDtoB;

/**
 * @author adrninistrator
 * @date 2024/2/19
 * @description:
 */
public class FREQCMethodReturnMCReturn {

    public static String returnString1() {
        FREQCDtoB freqcDtoB = new FREQCDtoB();
        return freqcDtoB.getString1().trim();
    }

    public static String returnString2() {
        FREQCDtoB freqcDtoB = new FREQCDtoB();
        return freqcDtoB.getString2().trim().trim();
    }

    public static String returnString3() {
        FREQCDtoB freqcDtoB = new FREQCDtoB();
        String str1 = freqcDtoB.getString2();
        if (System.getProperty("") != null) {
            return str1;
        }
        return str1.trim().trim();
    }

    public static String returnString4() {
        FREQCDtoB freqcDtoB = new FREQCDtoB();
        return freqcDtoB.getString3();
    }

    public static void test1() {
        FREQCDtoA freqcDtoA = new FREQCDtoA();
        freqcDtoA.setString1(returnString1());
        freqcDtoA.setString2(returnString2());
        freqcDtoA.setString2(returnString3());
        freqcDtoA.setString1(returnString4());
        freqcDtoA.setString3(returnString1().trim());
        freqcDtoA.setString2(returnString2().trim().trim());
        freqcDtoA.setString2(returnString3().trim().trim().trim());
        freqcDtoA.setString1(returnString4().trim().trim().trim().trim());
    }
}
