package test.callgraph.reflectioncustom.util;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author adrninistrator
 * @date 2025/11/9
 * @description:
 */
public class TestReflectionCustomUtil2 {

    private static final Logger logger = LoggerFactory.getLogger(TestReflectionCustomUtil2.class);

    public static void runByReflection(String threadName, Object obj, String methodName, Object... args) {
        logger.info("通过反射执行方法，使用线程 {} 对应被调用对象 {} 被调用方法 {}，被调用参数使用 args", threadName, obj.getClass().getName(), methodName);
    }

    private TestReflectionCustomUtil2() {
        throw new IllegalStateException("illegal");
    }
}
