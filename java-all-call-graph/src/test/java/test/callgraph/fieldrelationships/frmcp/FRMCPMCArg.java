package test.callgraph.fieldrelationships.frmcp;

import test.callgraph.fieldrelationships.frmcp.dto.FRMCPDtoA;
import test.callgraph.fieldrelationships.frmcp.dto.FRMCPDtoB;

/**
 * @author adrninistrator
 * @date 2024/1/30
 * @description:
 */
public class FRMCPMCArg {
    public static void test1(String str1) {
        FRMCPDtoA frMCPDtoA = new FRMCPDtoA();
        frMCPDtoA.setStr1(str1);
    }

    public static void test1A() {
        FRMCPDtoB frMCPDtoB = new FRMCPDtoB();
        test1(frMCPDtoB.getStr1());
    }

    public static void test2(String str1) {
        FRMCPDtoA frMCPDtoA = new FRMCPDtoA();
        frMCPDtoA.setStr2(str1);
    }

    public static void test2A(String str1) {
        test2(str1);
    }

    public static void test2AA() {
        FRMCPDtoB frMCPDtoB = new FRMCPDtoB();
        test2A(frMCPDtoB.getStr2());
    }

    public static void test2C() {
        FRMCPDtoB frMCPDtoB = new FRMCPDtoB();
        test2(frMCPDtoB.getStr3());
    }
}
