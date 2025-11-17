package test.callgraph.reflectioncustom.util;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author adrninistrator
 * @date 2025/2/15
 * @description:
 */
public class TestReflectionCustomUtil1 {

    private static final Logger logger = LoggerFactory.getLogger(TestReflectionCustomUtil1.class);

    public static void runByReflection(Object obj, String methodName, Object... args) {
        logger.info("通过反射执行方法，对应被调用对象 {} 被调用方法 {}，被调用参数使用 args", obj.getClass().getName(), methodName);
    }

    private TestReflectionCustomUtil1() {
        throw new IllegalStateException("illegal");
    }
}
