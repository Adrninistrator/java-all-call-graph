package test.callgraph.fieldrelationships.frmcp;

import test.callgraph.fieldrelationships.frmcp.dto.FRMCPDtoA;
import test.callgraph.fieldrelationships.frmcp.dto.FRMCPDtoB;

/**
 * @author adrninistrator
 * @date 2024/1/30
 * @description:
 */
public class FRMCPRtnMC {
    public static void test1() {
        FRMCPDtoA frMCPDtoA = new FRMCPDtoA();
        String str1 = test1A();
        frMCPDtoA.setStr1(str1);
    }

    public static String test1A() {
        FRMCPDtoB frMCPDtoB = new FRMCPDtoB();
        return frMCPDtoB.getStr1();
    }

    public static void test2() {
        FRMCPDtoA frMCPDtoA = new FRMCPDtoA();
        String str1 = test2A();
        frMCPDtoA.setStr1(str1);
    }

    public static String test2A() {
        return test2AA();
    }

    public static String test2AA() {
        FRMCPDtoB frMCPDtoB = new FRMCPDtoB();
        return frMCPDtoB.getStr2();
    }
}
