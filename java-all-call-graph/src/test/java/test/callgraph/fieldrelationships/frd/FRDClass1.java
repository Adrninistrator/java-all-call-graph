package test.callgraph.fieldrelationships.frd;

import org.apache.commons.lang3.StringUtils;
import test.callgraph.fieldrelationships.frf.FRFDtoC;

import java.math.BigDecimal;

/**
 * @author adrninistrator
 * @date 2023/8/4
 * @description:
 */
public class FRDClass1 {
    public void test1() {
        FRDDtoA frdDtoA = new FRDDtoA();
        FRDDtoB frdDtoB = new FRDDtoB();
        frdDtoB.setString1(StringUtils.substring(frdDtoA.getString1(), 1));
        frdDtoB.setBigDecimal1(frdDtoA.getBigDecimal1().multiply(BigDecimal.TEN));
    }

    public void test1a() {
        FRDDtoA frdDtoA = new FRDDtoA();
        FRDDtoB frdDtoB = new FRDDtoB();
        frdDtoB.setString1(frdDtoA.getString1().substring(1));
    }

    public void test1b() {
        FRDDtoA frdDtoA = new FRDDtoA();
        FRDDtoB frdDtoB = new FRDDtoB();
        Integer i = frdDtoA.getI1();
        frdDtoB.setI1(i + 1);
    }

    public void test2() {
        FRDDtoA frdDtoA = new FRDDtoA();
        FRDDtoB frdDtoB = new FRDDtoB();
        String s1 = StringUtils.substring(frdDtoA.getString1(), 1);
        frdDtoB.setString1(s1);
        BigDecimal b1 = frdDtoA.getBigDecimal1().multiply(BigDecimal.TEN);
        frdDtoB.setBigDecimal1(b1);
    }

    public void test2a() {
        FRDDtoA frdDtoA = new FRDDtoA();
        FRDDtoB frdDtoB = new FRDDtoB();
        frdDtoB.setString1("1" + frdDtoA.getString1() + "2");
    }

    public void test3() {
        FRDDtoA frdDtoA = new FRDDtoA();
        FRDDtoB frdDtoB = new FRDDtoB();
        frdDtoB.setI1(frdDtoA.getI1() + 1);
        frdDtoB.setI2(frdDtoA.getI2() - 1);
    }

    public void test4() {
        FRDDtoA frdDtoA = new FRDDtoA();
        FRDDtoB frdDtoB = new FRDDtoB();
        frdDtoB.setI1(frdDtoA.getI1() * frdDtoA.getI2());
        frdDtoB.setI2(frdDtoA.getI1() / frdDtoA.getI2());
    }

    public void test5() {
        FRDDtoA frdDtoA = new FRDDtoA();
        FRDDtoB frdDtoB = new FRDDtoB();
        frdDtoB.setI1(frdDtoA.getI1() + frdDtoA.getI2() + frdDtoA.getI3());
    }

    public void test6a() {
        FRDDtoA frdDtoA = new FRDDtoA();
        FRDDtoB frdDtoB = new FRDDtoB();
        FRFDtoC frdDtoC = test6b(frdDtoA.getString1());
        frdDtoB.setString1(frdDtoC.getString1() + "a");
    }

    public FRFDtoC test6b(String s1) {
        return new FRFDtoC();
    }

    public void test7() {
        FRDDtoA frdDtoA = new FRDDtoA();
        FRDDtoB frdDtoB = new FRDDtoB();
        BigDecimal bigDecimal1 = new BigDecimal(frdDtoA.getString1());
        frdDtoB.setBigDecimal1(bigDecimal1);

        String str1 = frdDtoA.getBigDecimal1().toString();
        String str2 = frdDtoA.getBigDecimal1().toPlainString();
        frdDtoB.setString1(str1);
        frdDtoB.setString1(str2);
    }

    public void test8() {
        FRDDtoA frdDtoA = new FRDDtoA();
        FRDDtoB frdDtoB = new FRDDtoB();
        frdDtoB.setFrdDtoA(frdDtoA);
    }
}
