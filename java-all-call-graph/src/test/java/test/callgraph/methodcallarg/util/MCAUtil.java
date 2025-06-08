package test.callgraph.methodcallarg.util;

import com.adrninistrator.javacg2.common.enums.JavaCG2CallTypeEnum;

/**
 * @author adrninistrator
 * @date 2025/2/17
 * @description:
 */
public class MCAUtil {

    public static void run(String str1, int int1, Object... args) {
    }

    public static void run2(JavaCG2CallTypeEnum javaCG2CallTypeEnum) {
    }

    public static String getFlag() {
        return "flag";
    }

    private MCAUtil() {
        throw new IllegalStateException("illegal");
    }
}
