package test.callgraph.methodcallarg.util;

/**
 * @author adrninistrator
 * @date 2025/2/17
 * @description:
 */
public class MCAUtil {

    public static void run(String str1, int int1) {
    }

    public static String getFlag() {
        return "flag";
    }

    private MCAUtil() {
        throw new IllegalStateException("illegal");
    }
}
