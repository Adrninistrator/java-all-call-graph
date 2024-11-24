package com.adrninistrator.jacg.neo4j.writedb;

import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodLineNumber;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4MethodLineNumber;
import com.adrninistrator.jacg.neo4j.domain.node.JACGMethodLineNumber;
import com.adrninistrator.jacg.neo4j.repository.JACGMethodLineNumberRepository;

/**
 * @author adrninistrator
 * @date 2024/7/24
 * @description:
 */
public class Neo4jWriteDbHandler4MethodLineNumber extends WriteDbHandler4MethodLineNumber {
    public Neo4jWriteDbHandler4MethodLineNumber(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected boolean useNeo4j() {
        return true;
    }

    @SuppressWarnings("unchecked")
    @Override
    protected Class chooseNeo4jRepository() {
        return JACGMethodLineNumberRepository.class;
    }

    @Override
    protected Object transferNeo4jDomain(WriteDbData4MethodLineNumber data) {
        JACGMethodLineNumber jacgMethodLineNumber = new JACGMethodLineNumber();
        jacgMethodLineNumber.setAppName(appName);
        jacgMethodLineNumber.setMethodHash(data.getMethodHash());
        jacgMethodLineNumber.setSimpleClassName(data.getSimpleClassName());
        jacgMethodLineNumber.setMethodName(data.getMethodName());
        jacgMethodLineNumber.setMinLineNumber(data.getMinLineNumber());
        jacgMethodLineNumber.setMaxLineNumber(data.getMaxLineNumber());
        jacgMethodLineNumber.setFullMethod(data.getFullMethod());
        return jacgMethodLineNumber;
    }
}
