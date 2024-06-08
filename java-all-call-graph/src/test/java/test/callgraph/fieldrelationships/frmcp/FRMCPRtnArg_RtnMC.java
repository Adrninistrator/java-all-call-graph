package test.callgraph.fieldrelationships.frmcp;

import test.callgraph.fieldrelationships.frmcp.dto.FRMCPDtoA;
import test.callgraph.fieldrelationships.frmcp.dto.FRMCPDtoB;

/**
 * @author adrninistrator
 * @date 2024/1/30
 * @description:
 */
public class FRMCPRtnArg_RtnMC {
    public static void test1(String arg) {
        String usedArg = arg;
        FRMCPDtoA frMCPDtoA = new FRMCPDtoA();
        if (System.getProperty("flag") != null) {
            usedArg = test1B();
        }
        String str1 = test1ARtnArg(usedArg);
        frMCPDtoA.setStr5(str1);
    }

    public static String test1ARtnArg(String str1) {
        return str1;
    }

    public static String test1B() {
        FRMCPDtoB frMCPDtoB = new FRMCPDtoB();
        return frMCPDtoB.getStr1();
    }

    public static void test1C() {
        FRMCPDtoB frMCPDtoB = new FRMCPDtoB();
        String arg = frMCPDtoB.getStr1();
        test1(arg);
    }
}
