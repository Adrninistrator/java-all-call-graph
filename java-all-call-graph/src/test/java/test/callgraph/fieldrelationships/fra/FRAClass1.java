package test.callgraph.fieldrelationships.fra;

/**
 * @author adrninistrator
 * @date 2023/6/9
 * @description:
 */
public class FRAClass1 {

    private String str1;

    private static String STRING1;

    private final FRADtoA fraDtoAField = new FRADtoA();
    private static final FRADtoA FRA_DTO_A_STATIC = new FRADtoA();

    public void test1(int arg1) {
        FRADtoA fraDtoA = new FRADtoA();
        FRADtoB fraDtoB = new FRADtoB();

        fraDtoB.intB1 = fraDtoA.intA1;
        fraDtoB.intB1 = arg1;
        fraDtoAField.longA1 = fraDtoA.longA1;
        fraDtoB.longB1 = fraDtoAField.longA1;
        FRA_DTO_A_STATIC.strA1 = fraDtoA.strA1;
        fraDtoB.strFieldB1 = FRA_DTO_A_STATIC.strA1;
    }

    public static void test2(int arg1) {
        FRADtoA fraDtoA = new FRADtoA();
        FRADtoB fraDtoB = new FRADtoB();

        fraDtoB.setIntB1(fraDtoA.getIntA1());
        fraDtoB.setIntB1(arg1);
        fraDtoB.setLongB1(fraDtoA.getLongA1());
        fraDtoB.setStrFieldB1(fraDtoA.getStrA1());
        fraDtoB.setStrFieldB1(FRADtoA.getTest());
    }

    public void test3() {
        FRADtoA fraDtoA = new FRADtoA();
        FRADtoB fraDtoB = new FRADtoB();

        int a1 = fraDtoA.getIntA1();
        Long a2 = fraDtoA.getLongA1();
        String a3 = fraDtoA.getStrA1();

        str1 = a3;

        fraDtoB.setIntB1(a1);
        fraDtoB.setLongB1(a2);
        fraDtoB.setStrFieldB1(str1);
    }

    public void test4() {
        FRADtoA fraDtoA = new FRADtoA();
        FRADtoB fraDtoB = new FRADtoB();
        String a1 = fraDtoA.getStrA1();
        String a2 = a1;
        testSet(a2, fraDtoB);
    }

    public void test5() {
        FRADtoA fraDtoA = new FRADtoA();
        FRADtoB fraDtoB = new FRADtoB();
        String a1 = fraDtoA.getStrA1();
        String a2 = a1;
        STRING1 = a2;
        fraDtoB.strFieldB1 = STRING1;
        fraDtoB.setStrFieldB1(STRING1);
    }

    private void testSet(String str, FRADtoB fraDtoB) {
        fraDtoB.setStrFieldB1(str);
    }
}
