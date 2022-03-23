package test.call_graph.argument;

import java.math.BigDecimal;

/**
 * @author adrninistrator
 * @date 2021/10/31
 * @description:
 */
public class TestArgument1 {

    public static void test() {
        TestBigDecimal(BigDecimal.ONE);
        TestBoolean(Boolean.FALSE);
        TestByte(Byte.MAX_VALUE);
        TestDouble(1.1D);
        TestFloat(1.2F);
        TestInteger(2);
        TestInteger(128);
        TestLong(3L);
        TestLong(129L);
        TestLong(5000L);
        TestShort(Short.MAX_VALUE);
        TestString("abc");
    }

    public static void TestBigDecimal(BigDecimal arg) {
    }

    public static void TestBoolean(Boolean arg) {
    }

    public static void TestByte(Byte arg) {
    }

    public static void TestDouble(Double arg) {
    }

    public static void TestFloat(Float arg) {
    }

    public static void TestInteger(Integer arg) {
    }

    public static void TestLong(Long arg) {
    }

    public static void TestShort(Short arg) {
    }

    public static void TestString(String arg) {
    }

}
