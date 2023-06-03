package test.run_by_code.handler.classes;

import com.adrninistrator.jacg.handler.classes.ClassInfoHandler;
import com.adrninistrator.javacg.dto.access_flag.JavaCGAccessFlags;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import test.call_graph.annotation.TestAnnotation;
import test.call_graph.implement.Interface1;
import test.call_graph.method_call.TestMCCaller;
import test.run_by_code.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2023/4/14
 * @description:
 */
public class TestClassInfoHandler extends TestRunByCodeBase {
    private static final Logger logger = LoggerFactory.getLogger(TestClassInfoHandler.class);

    @Test
    public void test() {
        try (ClassInfoHandler classInfoHandler = new ClassInfoHandler(configureWrapper)) {
            doTest(classInfoHandler, TestAnnotation.class.getName());
            doTest(classInfoHandler, Interface1.class.getName());
            doTest(classInfoHandler, TestMCCaller.class.getName());
        }
    }

    private void doTest(ClassInfoHandler classInfoHandler, String className) {
        JavaCGAccessFlags javaCGAccessFlags = classInfoHandler.queryClassJavaCGAccessFlags(className);
        logger.info("{} isInterface:{} isAnnotation:{} isAbstract:{}", className, javaCGAccessFlags.isInterface(), javaCGAccessFlags.isAnnotation(), javaCGAccessFlags.isAbstract());
        printObjectContent(javaCGAccessFlags, className);
    }
}
