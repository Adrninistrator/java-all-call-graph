package com.adrninistrator.jacg.neo4j.repository;

import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodInfo;
import com.adrninistrator.jacg.neo4j.domain.node.JACGMethodInfo;
import org.springframework.data.neo4j.annotation.Query;
import org.springframework.data.neo4j.repository.Neo4jRepository;
import org.springframework.data.repository.query.Param;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2024/7/24
 * @description:
 */
public interface JACGMethodInfoRepository extends Neo4jRepository<JACGMethodInfo, String> {

    @Query("MATCH (n:jacg_method_info) " +
            "WHERE n.appName = $appName" +
            " AND n.simpleClassName = $simpleClassName" +
            " AND n.fullMethod STARTS WITH $fullMethodPrefix" +
            " AND n.returnType = $returnType" +
            " RETURN n.methodHash")
    String queryMethodHashByPrefix(@Param("appName") String appName, @Param("simpleClassName") String simpleClassName, @Param("fullMethodPrefix") String fullMethodPrefix,
                                   @Param("returnType") String returnType);

    @Query("MATCH (n:jacg_method_info) " +
            "WHERE n.appName = $appName AND n.simpleClassName = $simpleClassName " +
            "RETURN n.fullMethod, n.returnType")
    List<WriteDbData4MethodInfo> queryFullMethodWithAppName(@Param("appName") String appName, @Param("simpleClassName") String simpleClassName);

    @Query("MATCH (n:jacg_method_info) " +
            "WHERE n.appName = $appName " +
            "AND n.simpleClassName = $simpleClassName " +
            "AND n.fullMethod STARTS WITH $fullMethodPrefix " +
            "RETURN n")
    List<WriteDbData4MethodInfo> queryMethodInfoByClassFullMethodPrefix(@Param("appName") String appName, @Param("simpleClassName") String simpleClassName,
                                                                        @Param("fullMethodPrefix") String fullMethodPrefix);
}
