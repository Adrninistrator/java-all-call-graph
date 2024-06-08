package test.callgraph.fieldrelationships.frg;

/**
 * @author adrninistrator
 * @date 2024/1/16
 * @description:
 */
public class FRGClass1 {

//    private static String getValue() {
//        FRGDtoB frgDtoB = new FRGDtoB();
//        return frgDtoB.getStr2();
//    }
//
//    public static void test1() {
//        String value = getValue();
//        int flag = (int) System.currentTimeMillis() % 7;
//        if (flag == 1) {
//            value = FRGClass3.transferTimeNotEq(value, "abc");
//        } else if (flag == 2) {
//            value = getValue();
//        } else if (flag == 3) {
//            FRGDtoB frgDtoB = new FRGDtoB();
//            value = FRGClass3.transferValue(frgDtoB.getStr2().trim(), "-");
//        } else {
//            FRGDtoB frgDtoB = new FRGDtoB();
//            value = frgDtoB.getStr1();
//        }
//        FRGClass2.test(Long.parseLong(value));
//    }
//
//    public static void test2() {
//        FRGDtoB frgDtoB = new FRGDtoB();
//        String value = FRGClass3.transferValue(frgDtoB.getStr2().trim(), "-");
//        FRGClass2.test(Long.parseLong(value.trim().trim()));
//    }
//
//    public static void test3() {
//        FRGDtoB frgDtoB = new FRGDtoB();
//        String value = FRGClass3.transferValueNoTrim(frgDtoB.getStr2(), "-");
//        FRGClass2.test(Long.parseLong(value));
//    }

    public static void test4a() {
        FRGDtoB frgDtoB = new FRGDtoB();
        String value = FRGClass3.transferValue2(frgDtoB.getStr2().trim());
        FRGClass2.test2(value.trim().trim());
    }

//    public static void test4b() {
//        FRGDtoB frgDtoB = new FRGDtoB();
//        String value = FRGClass3.transferValue2NoTrim(frgDtoB.getStr2().trim());
//        FRGClass2.test2(value.trim().trim());
//    }

//    public static void test5() {
//        FRGDtoB frgDtoB = new FRGDtoB();
//        String value = FRGClass3.transferValue2NoTrim(frgDtoB.getStr2());
//        FRGDtoA frgDtoA = new FRGDtoA();
//        frgDtoA.setStr1(value);
//        System.out.println(frgDtoA);
//    }
//
//    public static void test6() {
//        FRGDtoB frgDtoB = new FRGDtoB();
//        FRGDtoA frgDtoA = new FRGDtoA();
//        frgDtoA.setStr1(frgDtoB.getStr2().trim().trim());
//        System.out.println(frgDtoA);
//        System.out.println(frgDtoB);
//    }
}
