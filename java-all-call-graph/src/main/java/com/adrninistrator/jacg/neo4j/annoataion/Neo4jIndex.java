package com.adrninistrator.jacg.neo4j.annoataion;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * @author adrninistrator
 * @date 2024/7/26
 * @description: 自动创建Neo4j索引
 */
@Target({ElementType.TYPE})
@Retention(RetentionPolicy.RUNTIME)
public @interface Neo4jIndex {

    // 属性名称
    String[] properties();
}
