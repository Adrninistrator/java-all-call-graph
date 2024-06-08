package test.callgraph.fieldrelationships.fra;

/**
 * @author adrninistrator
 * @date 2024/1/28
 * @description:
 */
public class FRAClass4 {
    public static void testA() {
        FRADtoA fraDtoA = new FRADtoA();
        fraDtoA.setStrA1(test1());
        System.out.println(fraDtoA);
        fraDtoA.setStrA2(System.getProperty(""));
    }

    public static String test1() {
        return test2();
    }

    public static String test2() {
        return test3();
    }

    public static String test3() {
        FRADtoB fraDtoB = new FRADtoB();
        return fraDtoB.getStr2();
    }
}
