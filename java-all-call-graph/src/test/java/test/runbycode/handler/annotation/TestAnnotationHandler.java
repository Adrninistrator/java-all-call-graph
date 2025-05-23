package test.runbycode.handler.annotation;

import com.adrninistrator.jacg.dto.annotation.BaseAnnotationAttribute;
import com.adrninistrator.jacg.dto.annotation.SuperClassWithAnnotation;
import com.adrninistrator.jacg.dto.method.FullMethodWithReturnType;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4FieldAnnotation;
import com.adrninistrator.jacg.handler.annotation.AnnotationHandler;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.junit.Assert;
import org.junit.Test;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.stereotype.Controller;
import org.springframework.stereotype.Service;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import test.callgraph.annotation.TestAnnotation1;
import test.callgraph.methodargument.TestClassWithAnnotation2A;
import test.callgraph.methodargument.TestClassWithAnnotation3A;
import test.callgraph.spring.bean.define.impl.SpringServiceImplB2;
import test.runbycode.base.TestRunByCodeBase;

import javax.annotation.Resource;
import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2023/2/16
 * @description:
 */
public class TestAnnotationHandler extends TestRunByCodeBase {

    @Test
    public void $test0WriteDb() {
        commonWriteDb();
    }

    @Test
    public void testQueryClassesWithAnnotationsSimple() {
        try (AnnotationHandler annotationHandler = new AnnotationHandler(configureWrapper)) {
            List<String> simpleClassList = annotationHandler.queryClassesWithAnnotation(true, Configuration.class.getName());
            Assert.assertFalse(JavaCG2Util.isCollectionEmpty(simpleClassList));
            printListContent(simpleClassList, "simpleClassList");
        }
    }

    @Test
    public void testQueryClassesWithAnnotationsFull() {
        try (AnnotationHandler annotationHandler = new AnnotationHandler(configureWrapper)) {
            List<String> classList = annotationHandler.queryClassesWithAnnotation(false, RequestMapping.class.getName());
            Assert.assertFalse(JavaCG2Util.isCollectionEmpty(classList));
            printListContent(classList, "classList");
        }
    }

    @Test
    public void testQueryAnnotationAttributes4Class() {
        try (AnnotationHandler annotationHandler = new AnnotationHandler(configureWrapper)) {
            Map<String, BaseAnnotationAttribute> map = annotationHandler.queryAnnotationAttributes4Class(SpringServiceImplB2.class.getName(), Service.class.getName());
            Assert.assertFalse(JavaCG2Util.isMapEmpty(map));
            printMapContent(map, SpringServiceImplB2.class.getName(), Service.class.getName());

            map = annotationHandler.queryAnnotationAttributes4Class(SpringServiceImplB2.class.getName(), Controller.class.getName());
            Assert.assertTrue(JavaCG2Util.isMapEmpty(map));
            printMapContent(map, SpringServiceImplB2.class.getName(), Controller.class.getName());
        }
    }

    @Test
    public void testQueryMethodsWithAnnotationsFullMethod() {
        try (AnnotationHandler annotationHandler = new AnnotationHandler(configureWrapper)) {
            List<FullMethodWithReturnType> methodList = annotationHandler.queryMethodsWithAnnotation(PostMapping.class.getName());
            Assert.assertFalse(JavaCG2Util.isCollectionEmpty(methodList));
            printListContent(methodList, "methodList");
            for (FullMethodWithReturnType method : methodList) {
                Map<String, BaseAnnotationAttribute> map = annotationHandler.queryMethodAnnotationAttributes(method.getFullMethod(), method.getReturnType(),
                        PostMapping.class.getName());
                Assert.assertFalse(JavaCG2Util.isMapEmpty(map));
                printMapContent(map, method.getFullMethod(), method.getReturnType(), PostMapping.class.getName());
            }
        }
    }

    @Test
    public void testQueryMethodsWithAnnotationsMethodHash() {
        try (AnnotationHandler annotationHandler = new AnnotationHandler(configureWrapper)) {
            List<String> methodHashList = annotationHandler.queryMethodHashWithAnnotation(Bean.class.getName());
            Assert.assertFalse(JavaCG2Util.isCollectionEmpty(methodHashList));
            printListContent(methodHashList, "methodHashList");
        }
    }

    @Test
    public void testQuerySuperClassesInfo() {
        try (AnnotationHandler annotationHandler = new AnnotationHandler(configureWrapper)) {
            List<SuperClassWithAnnotation> superClassWithAnnotationList1 = annotationHandler.querySuperClassesInfo(TestClassWithAnnotation2A.class.getName(),
                    TestAnnotation1.class.getName());
            Assert.assertFalse(JavaCG2Util.isCollectionEmpty(superClassWithAnnotationList1));
            printListContent(superClassWithAnnotationList1, TestClassWithAnnotation2A.class.getName(), TestAnnotation1.class.getName());

            List<SuperClassWithAnnotation> superClassWithAnnotationList2 = annotationHandler.querySuperClassesInfo(TestClassWithAnnotation3A.class.getName(),
                    TestAnnotation1.class.getName());
            Assert.assertFalse(JavaCG2Util.isCollectionEmpty(superClassWithAnnotationList2));
            printListContent(superClassWithAnnotationList2, TestClassWithAnnotation3A.class.getName(), TestAnnotation1.class.getName());
        }
    }

    @Test
    public void testQueryClassFieldsWithAnnotation() {
        try (AnnotationHandler annotationHandler = new AnnotationHandler(configureWrapper)) {
            List<WriteDbData4FieldAnnotation> list = annotationHandler.queryClassFieldsWithAnnotation(Resource.class.getName());
            Assert.assertFalse(JavaCG2Util.isCollectionEmpty(list));
            printListContent(list);
            for (WriteDbData4FieldAnnotation fieldAnnotation : list) {
                Map<String, BaseAnnotationAttribute> map = annotationHandler.queryAnnotationAttributes4Field(fieldAnnotation.getClassName(), fieldAnnotation.getFieldName(),
                        Resource.class.getName());
                Assert.assertFalse(JavaCG2Util.isMapEmpty(map));
                printMapContent(map, fieldAnnotation.getClassName(), fieldAnnotation.getFieldName(), Resource.class.getName());
            }
        }
    }
}
