package test.call_graph.argument;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.math.BigDecimal;

/**
 * @author adrninistrator
 * @date 2021/10/31
 * @description:
 */
public class TestArgument1 {
    private static final Logger logger = LoggerFactory.getLogger(TestArgument1.class);

    public static void test() {
        logger.info("test\ra");
        System.out.println("test\ra");
        System.out.println("test\ra");
        System.out.println("test\ra");
        System.out.println("test\nb");
        System.out.println("test\r\nc");
        System.out.println("test\tc");
        testBigDecimal(BigDecimal.ONE);
        testBoolean(Boolean.FALSE);
        testByte(Byte.MAX_VALUE);
        testDouble(1.1D);
        testFloat(1.2F);
        testInteger(2);
        testInteger(128);
        testLong(3L);
        testLong(129L);
        testLong(5000L);
        testShort(Short.MAX_VALUE);
        testString("abc");
        testString("abc");
        testString("abc");
        testStringMulti("1", "2", "3");
        testStringMulti("1", "2", "3");
        testStringMulti("a", "b", "c");
        testStringArray("1", "2", "3");
        testStringArray("1", "2", "3");
        testStringArray("a", "b", "c");
    }

    public static void testBigDecimal(BigDecimal arg) {
        System.out.println(arg);
    }

    public static void testBoolean(Boolean arg) {
        arg = null;
        System.out.println(arg);
    }

    public static void testByte(Byte arg) {
        Byte arg2 = null;
        System.out.println(arg2);
    }

    public static void testDouble(Double arg) {
    }

    public static void testFloat(Float arg) {
    }

    public static void testInteger(Integer arg) {
    }

    public static void testLong(Long arg) {
    }

    public static void testShort(Short arg) {
    }

    public static void testString(String arg) {
    }

    public static void testStringMulti(String arg1, String arg2, String arg3) {
        System.out.println(arg1);
        System.out.println(arg2);
        System.out.println(arg3);
    }

    public static void testStringArray(String... args) {
        for (String arg : args) {
            System.out.println(arg);
        }
    }

    public static void testIntArray(int[] arg) {
        System.out.println(arg.length);
    }
}
