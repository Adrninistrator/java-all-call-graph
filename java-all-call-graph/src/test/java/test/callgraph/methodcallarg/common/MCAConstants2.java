package test.callgraph.methodcallarg.common;

/**
 * @author adrninistrator
 * @date 2025/2/17
 * @description:
 */
public class MCAConstants2 {

    public static final String FLAG2 = "flag2";

    public static final String FLAG_COMBINED = MCAConstants1.FLAG1 + "&" + FLAG2;

    private MCAConstants2() {
        throw new IllegalStateException("illegal");
    }
}
