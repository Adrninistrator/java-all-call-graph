package test.callgraph.reflection1.util;

/**
 * @author adrninistrator
 * @date 2025/10/10
 * @description:
 */
public class TestReflectionUtilWrapper1 {

    public static void runByReflection(Object obj, String methodName, Object... args) {
        TestReflectionUtil1.runByReflection(obj, methodName, args);
    }
}
