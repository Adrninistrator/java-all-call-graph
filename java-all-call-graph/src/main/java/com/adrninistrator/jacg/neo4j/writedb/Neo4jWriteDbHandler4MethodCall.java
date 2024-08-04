package com.adrninistrator.jacg.neo4j.writedb;

import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCall;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4MethodCall;
import com.adrninistrator.jacg.neo4j.domain.node.JACGMethodInMC;
import com.adrninistrator.jacg.neo4j.domain.relationship.JACGMethodCall;
import com.adrninistrator.jacg.neo4j.repository.JACGMethodCallRepository;
import com.adrninistrator.jacg.neo4j.repository.JACGMethodInMCRepository;
import com.adrninistrator.jacg.neo4j.util.JACGNeo4jUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2024/7/23
 * @description:
 */
public class Neo4jWriteDbHandler4MethodCall extends WriteDbHandler4MethodCall {

    private static final Logger logger = LoggerFactory.getLogger(Neo4jWriteDbHandler4MethodCall.class);

    private final Set<String> handledMethodHashSet = new HashSet<>();

    private JACGMethodCallRepository jacgMethodCallRepository;
    private JACGMethodInMCRepository jacgMethodInMCRepository;

    private int methodNum = 0;

    public Neo4jWriteDbHandler4MethodCall(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    public void init(WriteDbResult writeDbResult) {
        super.init(writeDbResult);
        jacgMethodCallRepository = applicationContext.getBean(JACGMethodCallRepository.class);
        jacgMethodInMCRepository = applicationContext.getBean(JACGMethodInMCRepository.class);
    }

    @Override
    protected boolean useNeo4j() {
        return true;
    }

    @Override
    protected Class chooseNeo4jRepository() {
        return JACGMethodCallRepository.class;
    }

    @Override
    protected boolean handleNeo4jDataList(List<WriteDbData4MethodCall> dataList) {
        List<JACGMethodInMC> methodList = new ArrayList<>();
        List<JACGMethodCall> methodCallList = new ArrayList<>();

        for (WriteDbData4MethodCall data : dataList) {
            JACGMethodInMC callerMethod = new JACGMethodInMC();
            callerMethod.setAppName(appName);
            callerMethod.setMethodHash(data.getCallerMethodHash());
            callerMethod.setSimpleClassName(data.getCallerSimpleClassName());
            callerMethod.setMethodName(data.getCallerMethodName());
            callerMethod.setFullMethod(data.getCallerFullMethod());
            callerMethod.setReturnType(data.getCallerReturnType());
            callerMethod.setJarNum(data.getCallerJarNum());
            // 对于已经保存到节点的方法，避免重复保存，下同
            if (handledMethodHashSet.add(data.getCallerMethodHash())) {
                methodList.add(callerMethod);
            }

            JACGMethodInMC calleeMethod = new JACGMethodInMC();
            calleeMethod.setAppName(appName);
            calleeMethod.setMethodHash(data.getCalleeMethodHash());
            calleeMethod.setSimpleClassName(data.getCalleeSimpleClassName());
            calleeMethod.setMethodName(data.getCalleeMethodName());
            calleeMethod.setFullMethod(data.getCalleeFullMethod());
            calleeMethod.setReturnType(data.getRawReturnType());
            calleeMethod.setJarNum(data.getCalleeJarNum());
            if (handledMethodHashSet.add(data.getCalleeMethodHash())) {
                methodList.add(calleeMethod);
            }

            JACGMethodCall methodCall = new JACGMethodCall();
            methodCall.setCallId(data.getCallId());
            methodCall.setCallType(data.getCallType());
            methodCall.setCalleeObjType(data.getCalleeObjType());
            methodCall.setEnabled(data.getEnabled());
            methodCall.setCallerLineNumber(data.getCallerLineNumber());
            methodCall.setCallFlags(data.getCallFlags());
            methodCall.setActualReturnType(data.getActualReturnType());
            methodCall.setCallerMethod(callerMethod);
            methodCall.setCalleeMethod(calleeMethod);
            methodCallList.add(methodCall);
        }
        if (!methodList.isEmpty()) {
            methodNum += methodList.size();
            JACGNeo4jUtil.saveAll(jacgMethodInMCRepository, methodList);
        }
        JACGNeo4jUtil.saveAll(jacgMethodCallRepository, methodCallList);
        return true;
    }

    @Override
    public boolean finalCheck() {
        logger.info("写入方法调用中的方法数量 {}", methodNum);
        return super.finalCheck();
    }
}
