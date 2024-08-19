package com.adrninistrator.jacg.neo4j.writedb;

import com.adrninistrator.jacg.dto.writedb.WriteDbData4ClassInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4ClassInfo;
import com.adrninistrator.jacg.neo4j.domain.node.JACGClassInfo;
import com.adrninistrator.jacg.neo4j.repository.JACGClassNameRepository;

/**
 * @author adrninistrator
 * @date 2024/7/23
 * @description:
 */
public class Neo4jWriteDbHandler4ClassInfo extends WriteDbHandler4ClassInfo {
    public Neo4jWriteDbHandler4ClassInfo(WriteDbResult writeDbResult) {
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
    protected Object transferNeo4jDomain(WriteDbData4ClassInfo data) {
        JACGClassInfo jacgClassInfo = new JACGClassInfo();
        jacgClassInfo.setAppName(appName);
        jacgClassInfo.setRecordId(data.getRecordId());
        jacgClassInfo.setSimpleClassName(data.getSimpleClassName());
        jacgClassInfo.setAccessFlags(data.getAccessFlags());
        jacgClassInfo.setClassName(data.getClassName());
        jacgClassInfo.setClassFileHash(data.getClassFileHash());
        jacgClassInfo.setJarNum(data.getJarNum());
        return jacgClassInfo;
    }
}