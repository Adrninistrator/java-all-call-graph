package test.callgraph.fieldrelationships.freqc;

import org.apache.commons.lang3.StringUtils;
import test.callgraph.fieldrelationships.freqc.dto.FREQCDtoA;
import test.callgraph.fieldrelationships.freqc.dto.FREQCDtoB;

import java.math.BigDecimal;

/**
 * @author adrninistrator
 * @date 2024/2/19
 * @description:
 */
public class FREQCGetSetInMethod {

    public void test1() {
        FREQCDtoA freqcDtoA = new FREQCDtoA();
        FREQCDtoB freqcDtoB = new FREQCDtoB();
        String str1 = freqcDtoA.getString1().trim();
        freqcDtoB.setString1(str1);
        freqcDtoB.setString1(StringUtils.trim(freqcDtoA.getString2()));
        freqcDtoB.setString2(freqcDtoA.getBigDecimal1().toString());
        freqcDtoB.setBigDecimal1(new BigDecimal(freqcDtoA.getString3()));
    }

    public void test2() {
        FREQCDtoA freqcDtoA = new FREQCDtoA();
        FREQCDtoB freqcDtoB = new FREQCDtoB();
        String str1 = freqcDtoA.getString1().trim();
        String str2 = str1.trim();
        freqcDtoB.setString1(str2);
        freqcDtoB.setString1(StringUtils.trim(freqcDtoA.getString2().trim()));
        freqcDtoB.setString2(freqcDtoA.getBigDecimal1().toString());
        freqcDtoB.setString3(new BigDecimal(freqcDtoA.getString3()).toPlainString());
    }

    public void test3() {
        FREQCDtoA freqcDtoA = new FREQCDtoA();
        FREQCDtoB freqcDtoB = new FREQCDtoB();
        boolean flag = System.getProperty("") != null;
        String str1 = flag ? freqcDtoA.getString1() : freqcDtoA.getString2();
        freqcDtoB.setString1(str1.trim());
    }

    public void test4() {
        FREQCDtoA freqcDtoA = new FREQCDtoA();
        FREQCDtoB freqcDtoB = new FREQCDtoB();
        boolean flag = System.getProperty("") != null;
        String str1 = flag ? freqcDtoA.getString1() : freqcDtoA.getString2();
        freqcDtoB.setString1(StringUtils.trim(str1));
    }
}
