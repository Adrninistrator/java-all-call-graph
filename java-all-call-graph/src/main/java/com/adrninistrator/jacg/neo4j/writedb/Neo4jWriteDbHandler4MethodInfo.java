package com.adrninistrator.jacg.neo4j.writedb;

import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4MethodInfo;
import com.adrninistrator.jacg.neo4j.domain.node.JACGMethodInfo;
import com.adrninistrator.jacg.neo4j.repository.JACGMethodInfoRepository;

/**
 * @author adrninistrator
 * @date 2024/7/24
 * @description:
 */
public class Neo4jWriteDbHandler4MethodInfo extends WriteDbHandler4MethodInfo {
    public Neo4jWriteDbHandler4MethodInfo(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected boolean useNeo4j() {
        return true;
    }

    @Override
    protected Class chooseNeo4jRepository() {
        return JACGMethodInfoRepository.class;
    }

    @Override
    protected Object transferNeo4jDomain(WriteDbData4MethodInfo data) {
        JACGMethodInfo jacgMethodInfo = new JACGMethodInfo();
        jacgMethodInfo.setAppName(appName);
        jacgMethodInfo.setMethodHash(data.getMethodHash());
        jacgMethodInfo.setSimpleClassName(data.getSimpleClassName());
        jacgMethodInfo.setAccessFlags(data.getAccessFlags());
        jacgMethodInfo.setMethodName(data.getMethodName());
        jacgMethodInfo.setFullMethod(data.getFullMethod());
        jacgMethodInfo.setSimpleReturnType(data.getSimpleReturnType());
        jacgMethodInfo.setReturnType(data.getReturnType());
        jacgMethodInfo.setMethodInstructionsHash(data.getMethodInstructionsHash());
        jacgMethodInfo.setJarNum(data.getJarNum());
        return jacgMethodInfo;
    }
}
