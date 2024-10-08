package test.callgraph.fieldrelationships.frmcp;

import com.adrninistrator.javacg2.dto.counter.JavaCG2Counter;
import test.callgraph.fieldrelationships.frmcp.dto.FRMCPDtoA;
import test.callgraph.fieldrelationships.frmcp.dto.FRMCPDtoB;

/**
 * @author adrninistrator
 * @date 2024/2/24
 * @description:
 */
public class FRMCPRtnMCCycle {
    public static void test1() {
        FRMCPDtoA frMCPDtoA = new FRMCPDtoA();
        String str1 = test1A(new JavaCG2Counter(0));
        frMCPDtoA.setStr1(str1);
    }

    public static String test1A(JavaCG2Counter counter) {
        if (counter.getCount() < 5) {
            counter.getCount();
            return test1A(counter);
        }

        FRMCPDtoB frMCPDtoB = new FRMCPDtoB();
        return frMCPDtoB.getStr1();
    }

    public static void test2() {
        FRMCPDtoA frMCPDtoA = new FRMCPDtoA();
        String str1 = test2A(new JavaCG2Counter(0));
        frMCPDtoA.setStr2(str1);
    }

    public static String test2A(JavaCG2Counter counter) {
        return test2B(counter);
    }

    public static String test2B(JavaCG2Counter counter) {
        if (counter.getCount() == 0) {
            counter.getCount();
            return test2C(counter);
        }

        FRMCPDtoB frMCPDtoB = new FRMCPDtoB();
        return frMCPDtoB.getStr2();
    }

    public static String test2C(JavaCG2Counter counter) {
        return test2A(counter);
    }
}
