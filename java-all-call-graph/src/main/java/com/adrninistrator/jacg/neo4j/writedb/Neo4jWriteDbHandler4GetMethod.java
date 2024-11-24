package com.adrninistrator.jacg.neo4j.writedb;

import com.adrninistrator.jacg.dto.writedb.WriteDbData4GetMethod;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4GetMethod;
import com.adrninistrator.jacg.neo4j.domain.node.JACGGetMethod;
import com.adrninistrator.jacg.neo4j.repository.JACGGetMethodRepository;

/**
 * @author adrninistrator
 * @date 2024/7/25
 * @description:
 */
public class Neo4jWriteDbHandler4GetMethod extends WriteDbHandler4GetMethod {
    public Neo4jWriteDbHandler4GetMethod(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected boolean useNeo4j() {
        return true;
    }

    @SuppressWarnings("unchecked")
    @Override
    protected Class chooseNeo4jRepository() {
        return JACGGetMethodRepository.class;
    }

    @Override
    protected Object transferNeo4jDomain(WriteDbData4GetMethod data) {
        JACGGetMethod jacgGetMethod = new JACGGetMethod();
        jacgGetMethod.setAppName(appName);
        copyWriteDbData(jacgGetMethod, data);
        return jacgGetMethod;
    }
}
