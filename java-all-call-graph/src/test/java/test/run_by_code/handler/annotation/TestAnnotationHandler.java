package test.run_by_code.handler.annotation;

import com.adrninistrator.jacg.dto.annotation.BaseAnnotationAttribute;
import com.adrninistrator.jacg.dto.annotation.SuperClassWithAnnotation;
import com.adrninistrator.jacg.handler.annotation.AnnotationHandler;
import org.junit.Test;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.stereotype.Controller;
import org.springframework.stereotype.Service;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import test.call_graph.annotation.TestAnnotation1;
import test.call_graph.argument.TestClassWithAnnotation2A;
import test.call_graph.argument.TestClassWithAnnotation3A;
import test.call_graph.spring.bean.define.impl.SpringServiceImplB2;
import test.run_by_code.base.TestRunByCodeBase;

import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2023/2/16
 * @description:
 */
public class TestAnnotationHandler extends TestRunByCodeBase {

    @Test
    public void testQueryClassesWithAnnotationsSimple() {
        try (AnnotationHandler annotationHandler = new AnnotationHandler(configureWrapper)) {
            List<String> simpleClassList = annotationHandler.queryClassesWithAnnotations(true, Configuration.class.getName());
            printListContent(simpleClassList, "simpleClassList");
        }
    }

    @Test
    public void testQueryClassesWithAnnotationsFull() {
        try (AnnotationHandler annotationHandler = new AnnotationHandler(configureWrapper)) {
            List<String> classList = annotationHandler.queryClassesWithAnnotations(false, RequestMapping.class.getName());
            printListContent(classList, "classList");
        }
    }

    @Test
    public void testQueryAnnotationAttributes4Class() {
        try (AnnotationHandler annotationHandler = new AnnotationHandler(configureWrapper)) {
            Map<String, BaseAnnotationAttribute> map = annotationHandler.queryAnnotationAttributes4Class(SpringServiceImplB2.class.getName(), Service.class.getName());
            printMapContent(map, SpringServiceImplB2.class.getName(), Service.class.getName());
            map = annotationHandler.queryAnnotationAttributes4Class(SpringServiceImplB2.class.getName(), Controller.class.getName());
            printMapContent(map, SpringServiceImplB2.class.getName(), Controller.class.getName());
        }
    }

    @Test
    public void testQueryMethodsWithAnnotationsFullMethod() {
        try (AnnotationHandler annotationHandler = new AnnotationHandler(configureWrapper)) {
            List<String> methodList = annotationHandler.queryMethodsWithAnnotations(true, PostMapping.class.getName());
            printListContent(methodList, "methodList");
        }
    }

    @Test
    public void testQueryMethodsWithAnnotationsMethodHash() {
        try (AnnotationHandler annotationHandler = new AnnotationHandler(configureWrapper)) {
            List<String> methodHashList = annotationHandler.queryMethodsWithAnnotations(false, Bean.class.getName());
            printListContent(methodHashList, "methodHashList");
        }
    }

    @Test
    public void testQuerySuperClassesInfo() {
        try (AnnotationHandler annotationHandler = new AnnotationHandler(configureWrapper)) {
            List<SuperClassWithAnnotation> superClassWithAnnotationList1 = annotationHandler.querySuperClassesInfo(TestClassWithAnnotation2A.class.getName(),
                    TestAnnotation1.class.getName());
            printListContent(superClassWithAnnotationList1, TestClassWithAnnotation2A.class.getName(), TestAnnotation1.class.getName());
            List<SuperClassWithAnnotation> superClassWithAnnotationList2 = annotationHandler.querySuperClassesInfo(TestClassWithAnnotation3A.class.getName(),
                    TestAnnotation1.class.getName());
            printListContent(superClassWithAnnotationList2, TestClassWithAnnotation3A.class.getName(), TestAnnotation1.class.getName());
        }
    }
}
