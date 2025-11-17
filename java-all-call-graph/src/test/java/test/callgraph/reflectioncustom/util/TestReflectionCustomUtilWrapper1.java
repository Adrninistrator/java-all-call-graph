package test.callgraph.reflectioncustom.util;

/**
 * @author adrninistrator
 * @date 2025/10/10
 * @description:
 */
public class TestReflectionCustomUtilWrapper1 {

    public static void runByReflection(Object obj, String methodName, Object... args) {
        TestReflectionCustomUtil1.runByReflection(obj, methodName, args);
    }
}
