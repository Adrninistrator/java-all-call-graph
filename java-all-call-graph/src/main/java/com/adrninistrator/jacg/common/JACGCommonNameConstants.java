package com.adrninistrator.jacg.common;

import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.fasterxml.jackson.annotation.JsonProperty;
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

    public static final String JAVAX_HTTP_SERVLET_RESPONSE = "javax.servlet.http.HttpServletResponse";

    public static final String JSON_PROPERTY_ANNOTATION_NAME_ = JsonProperty.class.getName();

    public static final String MYBATIS_PARAM_ANNOTATION_NAME = "org.apache.ibatis.annotations.Param";

    public static final String ANNOTATION_ATTRIBUTE_NAME_VALUE = "value";

    public static final String ENUM_METHOD_NAME = JavaCG2Constants.FLAG_COLON + "name()";

    private JACGCommonNameConstants() {
        throw new IllegalStateException("illegal");
    }
}
