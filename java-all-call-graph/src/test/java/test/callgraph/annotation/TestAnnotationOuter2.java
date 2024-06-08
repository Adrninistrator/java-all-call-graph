package test.callgraph.annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * @author adrninistrator
 * @date 2022/8/27
 * @description:
 */

@Target(ElementType.METHOD)
@Retention(RetentionPolicy.CLASS)
public @interface TestAnnotationOuter2 {
    String value();

    TestAnnotationOuter[] annotations();
}
