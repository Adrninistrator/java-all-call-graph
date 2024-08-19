package com.adrninistrator.jacg.neo4j.writedb;

import com.adrninistrator.jacg.dto.writedb.WriteDbData4ClassName;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4ClassName;
import com.adrninistrator.jacg.neo4j.domain.node.JACGClassName;
import com.adrninistrator.jacg.neo4j.repository.JACGClassNameRepository;

/**
 * @author adrninistrator
 * @date 2024/7/22
 * @description:
 */
public class Neo4jWriteDbHandler4ClassName extends WriteDbHandler4ClassName {
    public Neo4jWriteDbHandler4ClassName(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected boolean useNeo4j() {
        return true;
    }

    @Override
    protected Class chooseNeo4jRepository() {
        return JACGClassNameRepository.class;
    }

    @Override
    protected Object transferNeo4jDomain(WriteDbData4ClassName data) {
        JACGClassName jacgClassName = new JACGClassName();
        jacgClassName.setAppName(appName);
        jacgClassName.setRecordId(data.getRecordId());
        jacgClassName.setClassName(data.getClassName());
        jacgClassName.setSimpleClassName(data.getSimpleClassName());
        jacgClassName.setDuplicateClass(data.getDuplicateClass());
        return jacgClassName;
    }
}
