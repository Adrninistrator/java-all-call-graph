package test.call_graph.rpc_annotation.annotation;

import java.lang.annotation.*;

/**
 * @author adrninistrator
 * @date 2021/8/10
 * @description:
 */

@Target({ElementType.TYPE})
@Retention(RetentionPolicy.RUNTIME)
@Inherited
public @interface Annotation1 {

    String version();

    String serviceId();

    int value3();
}
