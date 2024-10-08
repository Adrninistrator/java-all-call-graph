package test.runbycode.handler.classes;

import com.adrninistrator.jacg.handler.classes.ClassInfoHandler;
import com.adrninistrator.javacg2.dto.accessflag.JavaCG2AccessFlags;
import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import test.callgraph.annotation.TestAnnotation;
import test.callgraph.implement.Interface1;
import test.callgraph.methodcall.TestMCCaller;
import test.runbycode.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2023/4/14
 * @description:
 */
public class TestClassInfoHandler extends TestRunByCodeBase {
    private static final Logger logger = LoggerFactory.getLogger(TestClassInfoHandler.class);

    @Test
    public void $test0WriteDb() {
        commonWriteDb();
    }

    @Test
    public void test() {
        try (ClassInfoHandler classInfoHandler = new ClassInfoHandler(configureWrapper)) {
            doTest(classInfoHandler, TestAnnotation.class.getName());
            doTest(classInfoHandler, Interface1.class.getName());
            doTest(classInfoHandler, TestMCCaller.class.getName());
        }
    }

    private void doTest(ClassInfoHandler classInfoHandler, String className) {
        JavaCG2AccessFlags javaCG2AccessFlags = classInfoHandler.queryClassJavaCG2AccessFlags(className);
        Assert.assertNotNull(javaCG2AccessFlags);
        logger.info("{} isInterface:{} isAnnotation:{} isAbstract:{}", className, javaCG2AccessFlags.isInterface(), javaCG2AccessFlags.isAnnotation(),
                javaCG2AccessFlags.isAbstract());
        printObjectContent(javaCG2AccessFlags, className);
    }
}
