package test.call_graph.rpc_annotation.annotation;

import java.lang.annotation.*;

/**
 * @author adrninistrator
 * @date 2021/8/15
 * @description:
 */

@Target({ElementType.FIELD})
@Retention(RetentionPolicy.RUNTIME)
@Inherited
public @interface FieldType {

    Type type();

    enum Type {
        SEQ,
        TIMEOUT,
        PROTOCOL
    }
}
