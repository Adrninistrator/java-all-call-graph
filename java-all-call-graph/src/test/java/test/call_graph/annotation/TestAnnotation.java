package test.call_graph.annotation;

import com.adrninistrator.jacg.common.enums.ConfigKeyEnum;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * @author adrninistrator
 * @date 2022/8/20
 * @description:
 */
@Target({ElementType.TYPE, ElementType.METHOD})
@Retention(RetentionPolicy.CLASS)
public @interface TestAnnotation {

    String strValue();

    int intValue();

    int[] intArrayValue();

    Class clazz1();

    ConfigKeyEnum enum1();

    TestAnnotationInner annotation1();
}
