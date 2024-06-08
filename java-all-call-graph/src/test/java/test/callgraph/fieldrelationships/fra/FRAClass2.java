package test.callgraph.fieldrelationships.fra;

import org.apache.commons.lang3.StringUtils;

/**
 * @author adrninistrator
 * @date 2023/6/11
 * @description:
 */
public class FRAClass2 {
    private String strField1 = "aaa";

    private static String STR_FIELD_2 = "bbb";

    public void test1() {
        FRADtoA fraDtoA = new FRADtoA();
        FRADtoB fraDtoB = new FRADtoB();
        int i1 = fraDtoA.getIntA1();
        int i2 = i1 + 1;
        fraDtoB.setIntB1(i2);

        String a2 = fraDtoA.getStrA2();
        test2(a2);
    }

    public void test2(String a2) {
        FRADtoA fraDtoA = new FRADtoA();
        FRADtoB fraDtoB = new FRADtoB();
        String str1 = fraDtoA.getStrA1();
        String str2 = str1 + "1";
        fraDtoB.setStrFieldB1(str2);
        fraDtoB.setStrFieldB1(str1.trim());
        fraDtoB.setStrFieldB1(StringUtils.trim(str1));
        fraDtoB.setStrFieldB1(a2.trim());
        fraDtoB.setStrFieldB1(StringUtils.trim(a2));
        fraDtoB.setStrFieldB1("aa".trim());
        fraDtoB.setStrFieldB1(StringUtils.trim("bb"));
        fraDtoB.setStrFieldB1(strField1.trim());
        fraDtoB.setStrFieldB1(StringUtils.trim(strField1));
        fraDtoB.setStrFieldB1(STR_FIELD_2.trim());
        fraDtoB.setStrFieldB1(StringUtils.trim(STR_FIELD_2));
    }

    public void test3() {
        FRADtoA fraDtoA = new FRADtoA();
        FRADtoB fraDtoB = new FRADtoB();
        boolean useStr1 = System.getProperty("test") != null;
        fraDtoB.setStrFieldB1(useStr1 ? fraDtoA.getStrA1() : fraDtoA.getStrA2());
    }

    public void test4() {
        FRADtoA fraDtoA = new FRADtoA();
        FRADtoB fraDtoB = new FRADtoB();
        boolean useStr1 = System.getProperty("test") != null;
        String str1 = fraDtoA.getStrA1();
        String str2 = fraDtoA.getStrA2();
        fraDtoB.setStrFieldB1(useStr1 ? str1 : str2);
    }

    public void test5() {
        FRADtoA fraDtoA = new FRADtoA();
        FRADtoC fraDtoC = new FRADtoC();
        fraDtoC.setStr3(fraDtoA.getStrA1());
        fraDtoC.setStr4(fraDtoA.getStrA2());
    }
}
