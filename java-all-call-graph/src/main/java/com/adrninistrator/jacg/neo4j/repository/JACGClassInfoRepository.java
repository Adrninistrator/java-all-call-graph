package com.adrninistrator.jacg.neo4j.repository;

import com.adrninistrator.jacg.neo4j.domain.node.JACGClassInfo;
import org.springframework.data.neo4j.repository.Neo4jRepository;

/**
 * @author adrninistrator
 * @date 2024/7/23
 * @description:
 */
public interface JACGClassInfoRepository extends Neo4jRepository<JACGClassInfo, String> {
}
