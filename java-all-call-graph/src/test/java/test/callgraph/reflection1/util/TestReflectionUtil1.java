package test.callgraph.reflection1.util;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author adrninistrator
 * @date 2025/2/15
 * @description:
 */
public class TestReflectionUtil1 {

    private static final Logger logger = LoggerFactory.getLogger(TestReflectionUtil1.class);

    public static void runByReflection(Object obj, String methodName, Object... args) {
        logger.info("通过反射执行方法，对应被调用对象 obj 被调用方法 methodName，被调用参数使用 args");
    }

    public static void runByReflection(String threadName, Object obj, String methodName, Object... args) {
        logger.info("通过反射执行方法，使用线程 threadName 对应被调用对象 obj 被调用方法 methodName，被调用参数使用 args");
    }

    private TestReflectionUtil1() {
        throw new IllegalStateException("illegal");
    }
}
