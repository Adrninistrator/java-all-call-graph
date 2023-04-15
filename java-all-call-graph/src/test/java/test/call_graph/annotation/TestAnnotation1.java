package test.call_graph.annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * @author adrninistrator
 * @date 2023/4/8
 * @description:
 */
@Target({ElementType.TYPE})
@Retention(RetentionPolicy.CLASS)
public @interface TestAnnotation1 {
    String name();

    String desc();
}
