package test.call_graph.rpc_annotation.annotation;

import java.lang.annotation.*;

/**
 * @author adrninistrator
 * @date 2021/8/15
 * @description:
 */
@Target({ElementType.TYPE})
@Retention(RetentionPolicy.RUNTIME)
@Inherited
public @interface AnnotationTimeout1 {

    long timeout() default 5000L;
}
