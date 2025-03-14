package test.callgraph.methodargument;

import test.callgraph.fieldrelationships.fra.FRADtoA;

import java.math.BigDecimal;

/**
 * @author adrninistrator
 * @date 2021/11/1
 * @description:
 */
public class TestArgument2 {

    private int fieldA = 1;

    public static void test() {
        int i = (int) System.currentTimeMillis() % 10;
        String s1 = (i == 7 ? "a" : "b");

        String s2 = "aa";
        int ii = 1;
        if (i == 7) {
            s2 = "bb";
        }

        testString(s1);
        testString(s2);

        TestArgument1.test1();
    }

    private static void testString(String s) {
    }

    private void testNoCallee() {
    }

    private void testNoCaller() {
        int i = (int) System.currentTimeMillis() % 10;
        int i2 = (int) System.currentTimeMillis() % 10;
        int i3 = (int) System.currentTimeMillis() % 10;
        String s1 = (i3 == 3 ? (i2 == 5 ? "a" : "b") : (i == 7 ? "c" : "d"));

        String s2 = "--";
        if (i3 == 1) {
            s2 = (i == 1 ? "aa" : "bb");
        } else if (i3 == 2) {
            s2 = (i == 2 ? "cc" : "dd");
        } else if (i3 == 3) {
            s2 = (i == 3 ? "ee" : "ff");
        } else if (i3 == 4) {
            s2 = (i == 4 ? "gg" : "hh");
        }

        System.setProperty(s1, s2);
    }

    private void test2() {
        TestArgument3 testArgument3 = new TestArgument3();
        testArgument3.test1(null, "a", 1);
        testArgument3.test1(1, "b", 2);
        testArgument3.test1(1.0, "c", 3);
        testArgument3.test1(1L, "d", 4);
        testArgument3.test1(BigDecimal.ONE, "e", 5);
        testArgument3.test1("test", "f", 6);
    }

    private void test3() {
        TestArgument3 testArgument3 = new TestArgument3();
        testArgument3.test2(new TestClassWithAnnotation1A(), "a", 1);
        testArgument3.test2(new TestClassWithAnnotation2A(), "b", 2);
    }

    private void test4(FRADtoA fraDtoA) {
        int a = 1;
        for (int i = 0; i < 10; i++) {
            if (a + i < 3) {
                System.getProperty("aaa");
            }
            if (a + i < i + 4) {
                System.getProperty("aaa");
            }
            if (a + i < i - fieldA) {
                System.getProperty("aaa");
            }
            if (a + i < i * fraDtoA.getIntA1()) {
                System.getProperty("aaa");
            }
            if (a + i < i / fraDtoA.getLongA1()) {
                System.getProperty("aaa");
            }
        }
    }
}
