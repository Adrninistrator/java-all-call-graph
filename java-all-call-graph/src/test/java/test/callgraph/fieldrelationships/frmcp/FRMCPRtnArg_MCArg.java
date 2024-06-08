package test.callgraph.fieldrelationships.frmcp;

import test.callgraph.fieldrelationships.frmcp.dto.FRMCPDtoA;
import test.callgraph.fieldrelationships.frmcp.dto.FRMCPDtoB;

/**
 * @author adrninistrator
 * @date 2024/1/30
 * @description:
 */
public class FRMCPRtnArg_MCArg {
    public static void test1(String arg) {
        FRMCPDtoA frMCPDtoA = new FRMCPDtoA();
        String str1 = test1ARtnArg(arg);
        frMCPDtoA.setStr1(str1);
    }

    public static String test1ARtnArg(String str1) {
        return str1;
    }

    public static void test1B(String arg) {
        FRMCPDtoB frMCPDtoB = new FRMCPDtoB();
        if (System.getProperty("flag") != null) {
            arg = frMCPDtoB.getStr1();
        }
        test1(arg);
    }

    public static void test1C() {
        FRMCPDtoB frMCPDtoB = new FRMCPDtoB();
        String arg = frMCPDtoB.getStr2();
        test1B(arg);
    }
}
