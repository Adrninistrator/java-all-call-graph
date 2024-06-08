package test.callgraph.fieldrelationships.frg;

/**
 * @author adrninistrator
 * @date 2024/1/16
 * @description:
 */
public class FRGClass2 {

//    public static void test(long longValue) {
//        FRGDtoA frgDtoA = new FRGDtoA();
//        frgDtoA.setLong1(Long.valueOf(longValue));
//        System.out.println(frgDtoA);
//    }

    public static void test2(String value) {
        FRGDtoA frgDtoA = new FRGDtoA();
        frgDtoA.setStr1(value);
        System.out.println(frgDtoA);
    }
}
