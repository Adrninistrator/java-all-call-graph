package test.callgraph.annotation;

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
@TestAnnotation1(name = "1", desc = "2")
public @interface TestAnnotation1A {
    String name();

    String desc();
}
