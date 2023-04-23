package test.run_by_code.handler.annotation;

import com.adrninistrator.jacg.dto.annotation.BaseAnnotationAttribute;
import com.adrninistrator.jacg.dto.annotation.SuperClassWithAnnotation;
import com.adrninistrator.jacg.handler.annotation.AnnotationHandler;
import com.adrninistrator.jacg.util.JACGJsonUtil;
import org.apache.commons.lang3.StringUtils;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Controller;
import org.springframework.stereotype.Service;
import test.call_graph.annotation.TestAnnotation1;
import test.call_graph.argument.TestClassWithAnnotation2A;
import test.call_graph.argument.TestClassWithAnnotation3A;
import test.call_graph.spring.bean.define.SpringServiceImplB2;
import test.run_by_code.base.TestRunByCodeBase;

import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2023/2/16
 * @description:
 */
public class TestAnnotationHandler extends TestRunByCodeBase {
    private static final Logger logger = LoggerFactory.getLogger(TestAnnotationHandler.class);

    @Test
    public void testQueryClassesWithAnnotationsSimple() {
        try (AnnotationHandler annotationHandler = new AnnotationHandler(configureWrapper)) {
            List<String> simpleClassList = annotationHandler.queryClassesWithAnnotations(true, "org.springframework.context.annotation.Configuration");
            logger.info("simpleClassList\n{}", StringUtils.join(simpleClassList, "\n"));
        }
    }

    @Test
    public void testQueryClassesWithAnnotationsFull() {
        try (AnnotationHandler annotationHandler = new AnnotationHandler(configureWrapper)) {
            List<String> classList = annotationHandler.queryClassesWithAnnotations(false, "org.springframework.web.bind.annotation.RequestMapping");
            logger.info("classList\n{}", StringUtils.join(classList, "\n"));
        }
    }

    @Test
    public void testQueryAnnotationAttributes4Class() {
        try (AnnotationHandler annotationHandler = new AnnotationHandler(configureWrapper)) {
            Map<String, BaseAnnotationAttribute> map = annotationHandler.queryAnnotationAttributes4Class(SpringServiceImplB2.class.getName(), Service.class.getName());
            logger.info("map\n{}", JACGJsonUtil.getJsonStr(map));
            map = annotationHandler.queryAnnotationAttributes4Class(SpringServiceImplB2.class.getName(), Controller.class.getName());
            logger.info("map.isEmpty(): {}", map.isEmpty());
        }
    }

    @Test
    public void testQueryMethodsWithAnnotationsFullMethod() {
        try (AnnotationHandler annotationHandler = new AnnotationHandler(configureWrapper)) {
            List<String> methodList = annotationHandler.queryMethodsWithAnnotations(true, "org.springframework.web.bind.annotation.PostMapping");
            logger.info("methodList\n{}", StringUtils.join(methodList, "\n"));
        }
    }

    @Test
    public void testQueryMethodsWithAnnotationsMethodHash() {
        try (AnnotationHandler annotationHandler = new AnnotationHandler(configureWrapper)) {
            List<String> methodHashList = annotationHandler.queryMethodsWithAnnotations(false, "org.springframework.context.annotation.Bean");
            logger.info("methodHashList\n{}", StringUtils.join(methodHashList, "\n"));
        }
    }

    @Test
    public void testQuerySuperClassesInfo() {
        try (AnnotationHandler annotationHandler = new AnnotationHandler(configureWrapper)) {
            List<SuperClassWithAnnotation> superClassWithAnnotationList1 = annotationHandler.querySuperClassesInfo(TestClassWithAnnotation2A.class.getName(),
                    TestAnnotation1.class.getName());
            logger.info("superClassWithAnnotationList1\n{}", JACGJsonUtil.getJsonStr(superClassWithAnnotationList1));
            List<SuperClassWithAnnotation> superClassWithAnnotationList2 = annotationHandler.querySuperClassesInfo(TestClassWithAnnotation3A.class.getName(),
                    TestAnnotation1.class.getName());
            logger.info("superClassWithAnnotationList2\n{}", JACGJsonUtil.getJsonStr(superClassWithAnnotationList2));
        }
    }
}
