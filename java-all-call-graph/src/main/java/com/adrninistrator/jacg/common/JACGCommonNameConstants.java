package com.adrninistrator.jacg.common;

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

    public static final String[] CLASS_NAMES_STREAM = {
            Stream.class.getName(),
            DoubleStream.class.getName(),
            IntStream.class.getName(),
            LongStream.class.getName()
    };

    public static final String[] METHOD_NAMES_STREAM_INTERMEDIATE = {
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

    public static final String[] METHOD_NAMES_STREAM_TERMINAL = {
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

    public static final String SPRING_TRANSACTION_TEMPLATE_CLASS = "org.springframework.transaction.support.TransactionTemplate";

    public static final String SPRING_TX_ANNOTATION = "org.springframework.transaction.annotation.Transactional";

    public static final String SPRING_TX_ATTRIBUTE_PROPAGATION = "propagation";

    public static final String SPRING_ASYNC_ANNOTATION = "org.springframework.scheduling.annotation.Async";

    private JACGCommonNameConstants() {
        throw new IllegalStateException("illegal");
    }
}
