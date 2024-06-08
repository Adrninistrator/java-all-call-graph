package test.callgraph.fieldrelationships.freqc;

import test.callgraph.fieldrelationships.freqc.dto.FREQCDtoA;
import test.callgraph.fieldrelationships.freqc.dto.FREQCDtoB;

/**
 * @author adrninistrator
 * @date 2024/2/19
 * @description:
 */
public class FREQCMCArgUseMCReturn {

    public static String returnString1() {
        FREQCDtoB freqcDtoB = new FREQCDtoB();
        return freqcDtoB.getString1();
    }

    public static void test1() {
        FREQCDtoA freqcDtoA = new FREQCDtoA();
        String str1 = returnString1().trim();
        freqcDtoA.setString1(str1);
    }

    public static void test2() {
        FREQCDtoA freqcDtoA = new FREQCDtoA();
        String str1 = returnString1().trim().trim();
        freqcDtoA.setString1(str1);
    }
}
