package com.adrninistrator.jacg.neo4j.repository;

import com.adrninistrator.jacg.neo4j.domain.node.JACGMethodInMC;
import org.springframework.data.neo4j.annotation.Query;
import org.springframework.data.neo4j.repository.Neo4jRepository;
import org.springframework.data.repository.query.Param;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2024/7/21
 * @description:
 */
public interface JACGMethodInMCRepository extends Neo4jRepository<JACGMethodInMC, String> {
    @Query("MATCH (caller:jacg_method_in_method_call)-[:jacg_method_call]->(callee:jacg_method_in_method_call) " +
            "WHERE caller.appName = $appName AND caller.simpleClassName = $simpleClassName " +
            "RETURN DISTINCT caller.fullMethod")
    List<String> queryDupClassNameByFlag(@Param("appName") String appName, @Param("simpleClassName") String simpleClassName);

    @Query("MATCH (caller:jacg_method_in_method_call)-[:jacg_method_call]->(callee:jacg_method_in_method_call) " +
            "WHERE caller.appName = $appName AND caller.simpleClassName = $simpleClassName " +
            "RETURN caller.fullMethod " +
            "LIMIT 1")
    String queryOneFullMethodByCallerSCN(@Param("appName") String appName, @Param("simpleClassName") String simpleClassName);
}
