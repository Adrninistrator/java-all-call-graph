package com.adrninistrator.jacg.neo4j.writedb;

import com.adrninistrator.jacg.dto.writedb.WriteDbData4SetMethod;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4SetMethod;
import com.adrninistrator.jacg.neo4j.domain.node.JACGSetMethod;
import com.adrninistrator.jacg.neo4j.repository.JACGSetMethodRepository;

/**
 * @author adrninistrator
 * @date 2024/7/25
 * @description:
 */
public class Neo4jWriteDbHandler4SetMethod extends WriteDbHandler4SetMethod {
    public Neo4jWriteDbHandler4SetMethod(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected boolean useNeo4j() {
        return true;
    }

    @Override
    protected Class chooseNeo4jRepository() {
        return JACGSetMethodRepository.class;
    }

    @Override
    protected Object transferNeo4jDomain(WriteDbData4SetMethod data) {
        JACGSetMethod jacgSetMethod = new JACGSetMethod();
        jacgSetMethod.setAppName(appName);
        jacgSetMethod.setRecordId(data.getRecordId());
        jacgSetMethod.setSimpleClassName(data.getSimpleClassName());
        jacgSetMethod.setMethodName(data.getMethodName());
        jacgSetMethod.setFieldName(data.getFieldName());
        jacgSetMethod.setFieldCategory(data.getFieldCategory());
        jacgSetMethod.setSimpleFieldType(data.getSimpleFieldType());
        jacgSetMethod.setFieldType(data.getFieldType());
        jacgSetMethod.setClassName(data.getClassName());
        jacgSetMethod.setMethodHash(data.getMethodHash());
        jacgSetMethod.setFullMethod(data.getFullMethod());
        return jacgSetMethod;
    }
}
