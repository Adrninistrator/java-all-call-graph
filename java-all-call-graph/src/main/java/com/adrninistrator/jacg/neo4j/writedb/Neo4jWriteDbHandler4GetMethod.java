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

    @Override
    protected Class chooseNeo4jRepository() {
        return JACGGetMethodRepository.class;
    }

    @Override
    protected Object transferNeo4jDomain(WriteDbData4GetMethod data) {
        JACGGetMethod jacgGetMethod = new JACGGetMethod();
        jacgGetMethod.setAppName(appName);
        jacgGetMethod.setRecordId(data.getRecordId());
        jacgGetMethod.setSimpleClassName(data.getSimpleClassName());
        jacgGetMethod.setMethodName(data.getMethodName());
        jacgGetMethod.setFieldName(data.getFieldName());
        jacgGetMethod.setFieldCategory(data.getFieldCategory());
        jacgGetMethod.setSimpleFieldType(data.getSimpleFieldType());
        jacgGetMethod.setFieldType(data.getFieldType());
        jacgGetMethod.setClassName(data.getClassName());
        jacgGetMethod.setMethodHash(data.getMethodHash());
        jacgGetMethod.setFullMethod(data.getFullMethod());
        return jacgGetMethod;
    }
}
