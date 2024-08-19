package com.adrninistrator.jacg.neo4j.writedb;

import com.adrninistrator.jacg.dto.writedb.WriteDbData4ClassReference;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4ClassReference;
import com.adrninistrator.jacg.neo4j.domain.node.JACGClassReference;
import com.adrninistrator.jacg.neo4j.repository.JACGClassNameRepository;

/**
 * @author adrninistrator
 * @date 2024/8/17
 * @description:
 */
public class Neo4jWriteDbHandler4ClassReference extends WriteDbHandler4ClassReference {
    public Neo4jWriteDbHandler4ClassReference(WriteDbResult writeDbResult) {
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
    protected Object transferNeo4jDomain(WriteDbData4ClassReference data) {
        JACGClassReference jacgClassReference = new JACGClassReference();
        jacgClassReference.setAppName(appName);
        jacgClassReference.setRecordId(data.getRecordId());
        jacgClassReference.setClassName(data.getClassName());
        jacgClassReference.setSimpleClassName(data.getSimpleClassName());
        jacgClassReference.setReferencedClassName(data.getReferencedClassName());
        jacgClassReference.setReferencedSimpleClassName(data.getReferencedSimpleClassName());
        return jacgClassReference;
    }
}
