package com.adrninistrator.jacg.neo4j.repository;

import com.adrninistrator.jacg.neo4j.domain.node.JACGMethodLineNumber;
import org.springframework.data.neo4j.annotation.Query;
import org.springframework.data.neo4j.repository.Neo4jRepository;
import org.springframework.data.repository.query.Param;

/**
 * @author adrninistrator
 * @date 2024/7/24
 * @description:
 */
public interface JACGMethodLineNumberRepository extends Neo4jRepository<JACGMethodLineNumber, String> {

    @Query("MATCH (n:jacg_method_line_number) " +
            "WHERE n.appName = $appName AND n.simpleClassName = $simpleClassName " +
            "AND n.minLineNumber <= $methodLineNum AND n.maxLineNumber >= $methodLineNum " +
            "RETURN n " +
            "LIMIT 1")
    JACGMethodLineNumber queryMethodLineNumber(@Param("appName") String appName, @Param("simpleClassName") String simpleClassName, @Param("methodLineNum") int methodLineNum);
}
