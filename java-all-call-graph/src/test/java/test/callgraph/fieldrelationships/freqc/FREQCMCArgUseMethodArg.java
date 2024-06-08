package test.callgraph.fieldrelationships.freqc;

import test.callgraph.fieldrelationships.freqc.dto.FREQCDtoA;
import test.callgraph.fieldrelationships.freqc.dto.FREQCDtoB;

/**
 * @author adrninistrator
 * @date 2024/2/19
 * @description:
 */
public class FREQCMCArgUseMethodArg {

    public static void useArg1(String str1) {
        FREQCDtoB freqcDtoB = new FREQCDtoB();
        String strUsed = str1.trim();
        freqcDtoB.setString1(strUsed);
    }

    public static void useArg2(String str1) {
        FREQCDtoB freqcDtoB = new FREQCDtoB();
        String strUsed = str1.trim().trim();
        freqcDtoB.setString1(strUsed);
    }

    public static void test1() {
        FREQCDtoA freqcDtoA = new FREQCDtoA();
        useArg1(freqcDtoA.getString1());
        useArg2(freqcDtoA.getString2());
    }
}
