package test.callgraph.elexample.annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * @author adrninistrator
 * @date 2025/9/26
 * @description:
 */
@Target({ElementType.METHOD})
@Retention(RetentionPolicy.CLASS)
public @interface TestExampleAnnotation1 {

    String strValue() default "";

    int intValue() default 0;
}
