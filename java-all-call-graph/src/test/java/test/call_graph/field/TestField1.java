package test.call_graph.field;

import java.math.BigDecimal;

/**
 * @author adrninistrator
 * @date 2022/9/17
 * @description:
 */
public class TestField1 {
    public TestField2 testField2a = new TestField2();

    public TestField2 testField2b = new TestField2();

    private String data;

    private BigDecimal bigDecimal;

    private int int1;

    private static String STRING1 = "111";

    public void test() {
        testField2a.data = "a";

        System.out.println(testField2a.data);

        data = "b";
        bigDecimal = BigDecimal.ONE;
        int1 = 123;

        System.out.println(data);
        System.out.println(bigDecimal);
        System.out.println(int1);
    }

    public void test1() {
        data = "111";

        TestField1 testField1 = new TestField1();
        testField1.data = "222";
        System.out.println(data);
        System.out.println(testField1.data);
    }

    public void test2() {
        System.out.println(STRING1);
        STRING1 = "222";
        System.out.println(STRING1);

        System.out.println(TestField2.STRING1);
        TestField2.STRING1 = "bbb";
        System.out.println(TestField2.STRING1);
    }

    public void test3() {
        System.out.println(TestField2.INSTANCE);
    }

    public void test4() {
        TestField2 testField2;

        int i = (int) System.currentTimeMillis() % 10;
        if (i == 1) {
            testField2 = testField2a;
            testField2.test1();
        } else {
            testField2 = testField2b;
            testField2.test1();
        }
        testField2.test1();
    }
}
