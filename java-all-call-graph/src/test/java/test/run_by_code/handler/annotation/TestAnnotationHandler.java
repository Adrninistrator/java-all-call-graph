package test.run_by_code.handler.annotation;

import com.adrninistrator.jacg.handler.annotation.AnnotationHandler;
import com.adrninistrator.jacg.runner.RunnerWriteDb;
import org.apache.commons.lang3.StringUtils;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import test.run_by_code.base.TestRunByCodeBase;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/2/16
 * @description:
 */
public class TestAnnotationHandler extends TestRunByCodeBase {
    private static final Logger logger = LoggerFactory.getLogger(TestAnnotationHandler.class);

    @Test
    public void test() {
        new RunnerWriteDb().run(configureWrapper, javaCGConfigureWrapper);

        try (AnnotationHandler annotationHandler = new AnnotationHandler(configureWrapper)) {
            List<String> simpleClassList = annotationHandler.queryClassesWithAnnotations(true, "org.springframework.context.annotation.Configuration");
            logger.info("simpleClassList\n{}", StringUtils.join(simpleClassList, "\n"));
            List<String> classList = annotationHandler.queryClassesWithAnnotations(false, "org.springframework.web.bind.annotation.RequestMapping");
            logger.info("classList\n{}", StringUtils.join(classList, "\n"));
            List<String> methodList = annotationHandler.queryMethodsWithAnnotations(true, "org.springframework.web.bind.annotation.PostMapping");
            logger.info("methodList\n{}", StringUtils.join(methodList, "\n"));
            List<String> methodHashList = annotationHandler.queryMethodsWithAnnotations(false, "org.springframework.context.annotation.Bean");
            logger.info("methodHashList\n{}", StringUtils.join(methodHashList, "\n"));
        }
    }
}
