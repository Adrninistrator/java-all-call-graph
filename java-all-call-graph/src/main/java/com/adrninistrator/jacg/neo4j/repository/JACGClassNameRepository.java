package com.adrninistrator.jacg.neo4j.repository;

import com.adrninistrator.jacg.neo4j.domain.node.JACGClassName;
import org.springframework.data.neo4j.annotation.Query;
import org.springframework.data.neo4j.repository.Neo4jRepository;
import org.springframework.data.repository.query.Param;

import java.util.List;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2024/7/22
 * @description:
 */
public interface JACGClassNameRepository extends Neo4jRepository<JACGClassName, String> {

    @Query("MATCH (n:jacg_class_name {appName: $appName, simpleClassName: $duplicateSimpleClassName}) " +
            "SET n.simpleClassName = n.className, n.duplicateClass = $duplicateClass " +
            "RETURN count(n)")
    int updateSimpleClassName2Full(@Param("appName") String appName, @Param("duplicateSimpleClassName") String duplicateSimpleClassName,
                                   @Param("duplicateClass") int $duplicateClass);

    // WITH中的字段都需要指定别名
    @Query("MATCH (n:jacg_class_name {appName: $appName}) " +
            "WITH n.simpleClassName AS simpleClassName, COUNT(*) AS count " +
            "WHERE count > 1 " +
            "RETURN simpleClassName")
    Set<String> queryDupSimpleClassName(@Param("appName") String appName);

    @Query("MATCH (n:jacg_class_name {appName: $appName, duplicateClass: $duplicateClass}) " +
            "RETURN n.simpleClassName")
    List<String> queryDupClassNameByFlag(@Param("appName") String appName, @Param("duplicateClass") int $duplicateClass);

    @Query("MATCH (n:jacg_class_name {appName: $appName, className: $className}) " +
            "RETURN n.simpleClassName")
    String querySimpleClassNameByFull(@Param("appName") String appName, @Param("className") String className);

    @Query("MATCH (n:jacg_class_name {appName: $appName, simpleClassName: $simpleClassName}) " +
            "RETURN n.simpleClassName")
    String querySimpleClassNameBySimple(@Param("appName") String appName, @Param("simpleClassName") String simpleClassName);
}
