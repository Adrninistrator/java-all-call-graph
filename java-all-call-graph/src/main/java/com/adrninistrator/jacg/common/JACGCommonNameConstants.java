package com.adrninistrator.jacg.common;

import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.fasterxml.jackson.annotation.JsonProperty;
import org.aspectj.lang.ProceedingJoinPoint;
import org.springframework.aop.aspectj.MethodInvocationProceedingJoinPoint;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.support.TransactionTemplate;

import java.util.stream.DoubleStream;
import java.util.stream.IntStream;
import java.util.stream.LongStream;
import java.util.stream.Stream;

/**
 * @author adrninistrator
 * @date 2022/8/28
 * @description: 常用类常量
 */
public class JACGCommonNameConstants {

    public static final Class<?>[] JAVA_BASIC_WRAPPER_TYPES = new Class[]{
            Integer.class,
            Long.class,
            Float.class,
            Double.class,
            Byte.class,
            Character.class,
            Short.class,
            String.class,
            Boolean.class
    };

    public static final String[] SPRING_MVC_CONTROLLER_ANNOTATIONS = new String[]{
            "org.springframework.stereotype.Controller",
            "org.springframework.web.bind.annotation.RestController"
    };

    public static final String[] SPRING_MVC_MAPPING_ANNOTATIONS = new String[]{
            "org.springframework.web.bind.annotation.RequestMapping",
            "org.springframework.web.bind.annotation.DeleteMapping",
            "org.springframework.web.bind.annotation.GetMapping",
            "org.springframework.web.bind.annotation.PatchMapping",
            "org.springframework.web.bind.annotation.PostMapping",
            "org.springframework.web.bind.annotation.PutMapping"
    };

    public static final String[] SPRING_MVC_MAPPING_ATTRIBUTE_NAMES = new String[]{
            "value",
            "path"
    };

    public static final String[] STREAM_CLASS_NAMES = {
            Stream.class.getName(),
            DoubleStream.class.getName(),
            IntStream.class.getName(),
            LongStream.class.getName()
    };

    public static final String[] STREAM_INTERMEDIATE_METHOD_NAMES = {
            "filter",
            "map",
            "mapToInt",
            "mapToLong",
            "mapToDouble",
            "flatMap",
            "flatMapToInt",
            "flatMapToLong",
            "flatMapToDouble",
            "distinct",
            "sorted",
            "peek",
            "limit",
            "skip",
            "mapToObj",
            "asLongStream",
            "asDoubleStream",
            "boxed",
            "sequential",
            "parallel",
            "mapToInt"
    };

    public static final String[] STREAM_TERMINAL_METHOD_NAMES = {
            "forEach",
            "forEachOrdered",
            "toArray",
            "reduce",
            "collect",
            "min",
            "max",
            "count",
            "anyMatch",
            "allMatch",
            "noneMatch",
            "findFirst",
            "findAny",
            "sum",
            "average",
            "summaryStatistics",
            "iterator",
            "spliterator"
    };

    public static final String SPRING_TRANSACTION_TEMPLATE_CLASS = TransactionTemplate.class.getName();
    public static final String SPRING_TX_ANNOTATION = Transactional.class.getName();
    public static final String SPRING_TX_ATTRIBUTE_PROPAGATION = "propagation";
    public static final String SPRING_ASYNC_ANNOTATION = "org.springframework.scheduling.annotation.Async";

    public static final String SPRING_MULTI_PART_FILE_CLASS = "org.springframework.web.multipart.MultipartFile";
    public static final String SPRING_COMMONS_MULTI_PART_FILE_CLASS = "org.springframework.web.multipart.commons.CommonsMultipartFile";
    public static final String SPRING_HTTP_ENTITY_CLASS = "org.springframework.http.HttpEntity";
    public static final String SPRING_RESPONSE_ENTITY_CLASS = "org.springframework.http.ResponseEntity";

    public static final String SPRING_TASK_ANNOTATION = "org.springframework.scheduling.annotation.Scheduled";

    public static final String SPRING_BOOT_CONDITIONAL_ON_CLASS = "org.springframework.boot.autoconfigure.condition.ConditionalOnClass";

    public static final String JAVAX_HTTP_SERVLET_RESPONSE = "javax.servlet.http.HttpServletResponse";

    public static final String JSON_PROPERTY_ANNOTATION_NAME_ = JsonProperty.class.getName();

    public static final String MYBATIS_PARAM_ANNOTATION_NAME = "org.apache.ibatis.annotations.Param";

    public static final String ANNOTATION_ATTRIBUTE_NAME_VALUE = "value";
    public static final String ANNOTATION_ATTRIBUTE_NAME_NAME = "name";

    public static final String ENUM_METHOD_NAME = JavaCG2Constants.FLAG_COLON + "name()";

    public static final String[] SPRING_AOP_PROCEEDING_JOIN_POINT_NAMES = new String[]{
            ProceedingJoinPoint.class.getName(),
            MethodInvocationProceedingJoinPoint.class.getName()
    };

    public static final String CLASS_NAME_STRING_BUILDER = StringBuilder.class.getName();
    public static final String CLASS_NAME_STRING_BUFFER = StringBuffer.class.getName();

    public static final String METHOD_NAME_PROCEED = "proceed";
    public static final String METHOD_NAME_CLONE = "clone";
    public static final String METHOD_NAME_APPEND = "append";
    public static final String METHOD_NAME_TO_STRING = "toString";
    public static final String METHOD_NAME_GET_NAME = "getName";
    public static final String METHOD_NAME_GET_SIMPLE_NAME = "getSimpleName";

    public static final String METHOD_NAME_WITH_ARG_TYPE_CLONE = METHOD_NAME_CLONE + JavaCG2Constants.FLAG_LEFT_RIGHT_BRACKET;
    public static final String METHOD_NAME_WITH_ARG_TYPE_GET_NAME = METHOD_NAME_GET_NAME + JavaCG2Constants.FLAG_LEFT_RIGHT_BRACKET;
    public static final String METHOD_NAME_WITH_ARG_TYPE_GET_SIMPLE_NAME = METHOD_NAME_GET_SIMPLE_NAME + JavaCG2Constants.FLAG_LEFT_RIGHT_BRACKET;

    private JACGCommonNameConstants() {
        throw new IllegalStateException("illegal");
    }
}
