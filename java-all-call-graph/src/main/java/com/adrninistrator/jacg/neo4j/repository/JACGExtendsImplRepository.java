package com.adrninistrator.jacg.neo4j.repository;

import com.adrninistrator.jacg.neo4j.domain.node.JACGExtendsImpl;
import org.springframework.data.neo4j.annotation.Query;
import org.springframework.data.neo4j.repository.Neo4jRepository;
import org.springframework.data.repository.query.Param;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2024/7/23
 * @description:
 */
public interface JACGExtendsImplRepository extends Neo4jRepository<JACGExtendsImpl, String> {

    @Query("MATCH (n:jacg_extends_impl {appName: $appName, upwardSimpleClassName: $simpleClassName}) " +
            "RETURN n")
    List<JACGExtendsImpl> queryDownloadBySimple(@Param("appName") String appName, @Param("simpleClassName") String simpleClassName);
}