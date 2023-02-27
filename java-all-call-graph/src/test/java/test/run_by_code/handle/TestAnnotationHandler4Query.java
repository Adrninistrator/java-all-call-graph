package test.run_by_code.handle;

import com.adrninistrator.jacg.handler.annotation.AnnotationHandler4Query;
import com.adrninistrator.jacg.runner.RunnerWriteDb;
import org.apache.commons.lang3.StringUtils;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import test.run_by_code.TestRunByCodeBase;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/2/16
 * @description:
 */
public class TestAnnotationHandler4Query extends TestRunByCodeBase {
    private static final Logger logger = LoggerFactory.getLogger(TestAnnotationHandler4Query.class);

    @Test
    public void test() {
        new RunnerWriteDb().run(configureWrapper, javaCGConfigureWrapper);

        try (AnnotationHandler4Query annotationHandler4Query = new AnnotationHandler4Query(configureWrapper)) {
            List<String> simpleClassList = annotationHandler4Query.queryClassesWithAnnotations(true, "org.springframework.context.annotation.Configuration");
            logger.info("simpleClassList\n{}", StringUtils.join(simpleClassList, "\n"));
            List<String> classList = annotationHandler4Query.queryClassesWithAnnotations(false, "org.springframework.web.bind.annotation.RequestMapping");
            logger.info("classList\n{}", StringUtils.join(classList, "\n"));
            List<String> methodList = annotationHandler4Query.queryMethodsWithAnnotations(true, "org.springframework.web.bind.annotation.PostMapping");
            logger.info("methodList\n{}", StringUtils.join(methodList, "\n"));
            List<String> methodHashList = annotationHandler4Query.queryMethodsWithAnnotations(false, "org.springframework.context.annotation.Bean");
            logger.info("methodHashList\n{}", StringUtils.join(methodHashList, "\n"));
        }
    }
}
