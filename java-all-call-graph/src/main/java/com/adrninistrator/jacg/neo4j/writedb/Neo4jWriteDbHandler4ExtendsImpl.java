package com.adrninistrator.jacg.neo4j.writedb;

import com.adrninistrator.jacg.dto.writedb.WriteDbData4ExtendsImpl;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4ExtendsImpl;
import com.adrninistrator.jacg.neo4j.domain.node.JACGExtendsImpl;
import com.adrninistrator.jacg.neo4j.repository.JACGExtendsImplRepository;

/**
 * @author adrninistrator
 * @date 2024/7/23
 * @description:
 */
public class Neo4jWriteDbHandler4ExtendsImpl extends WriteDbHandler4ExtendsImpl {
    public Neo4jWriteDbHandler4ExtendsImpl(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected boolean useNeo4j() {
        return true;
    }

    @SuppressWarnings("unchecked")
    @Override
    protected Class chooseNeo4jRepository() {
        return JACGExtendsImplRepository.class;
    }

    @Override
    protected Object transferNeo4jDomain(WriteDbData4ExtendsImpl data) {
        JACGExtendsImpl jacgExtendsImpl = new JACGExtendsImpl();
        jacgExtendsImpl.setAppName(appName);
        jacgExtendsImpl.setRecordId(data.getRecordId());
        jacgExtendsImpl.setSimpleClassName(data.getSimpleClassName());
        jacgExtendsImpl.setClassName(data.getClassName());
        jacgExtendsImpl.setAccessFlags(data.getAccessFlags());
        jacgExtendsImpl.setType(data.getType());
        jacgExtendsImpl.setSeq(data.getSeq());
        jacgExtendsImpl.setExistsDownwardClasses(data.getExistsDownwardClasses());
        jacgExtendsImpl.setUpwardSimpleClassName(data.getUpwardSimpleClassName());
        jacgExtendsImpl.setUpwardClassName(data.getUpwardClassName());
        return jacgExtendsImpl;
    }
}
