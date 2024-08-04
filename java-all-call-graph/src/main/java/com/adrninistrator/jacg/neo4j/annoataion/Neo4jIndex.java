package com.adrninistrator.jacg.neo4j.annoataion;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

/**
 * @author adrninistrator
 * @date 2024/7/26
 * @description:
 */
@Retention(RetentionPolicy.RUNTIME)
public @interface Neo4jIndex {

    // 属性名称
    String[] properties();
}
