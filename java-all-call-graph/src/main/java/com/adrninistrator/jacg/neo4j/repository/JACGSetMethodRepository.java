package com.adrninistrator.jacg.neo4j.repository;

import com.adrninistrator.jacg.neo4j.domain.node.JACGGetMethod;
import org.springframework.data.neo4j.repository.Neo4jRepository;

/**
 * @author adrninistrator
 * @date 2024/7/25
 * @description:
 */
public interface JACGSetMethodRepository extends Neo4jRepository<JACGGetMethod, String> {
}
