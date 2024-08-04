package com.adrninistrator.jacg.neo4j.repository;

import com.adrninistrator.jacg.neo4j.domain.node.JACGMethodInfo;
import org.springframework.data.neo4j.annotation.Query;
import org.springframework.data.neo4j.repository.Neo4jRepository;
import org.springframework.data.repository.query.Param;

/**
 * @author adrninistrator
 * @date 2024/7/24
 * @description:
 */
public interface JACGMethodInfoRepository extends Neo4jRepository<JACGMethodInfo, String> {

    @Query("MATCH (n:jacg_method_info) " +
            "WHERE n.appName = $appName AND n.simpleClassName = $simpleClassName AND n.fullMethod STARTS WITH $fullMethodPrefix " +
            "RETURN n.methodHash")
    String queryMethodHashByPrefix(@Param("appName") String appName, @Param("simpleClassName") String simpleClassName, @Param("fullMethodPrefix") String fullMethodPrefix);
}
