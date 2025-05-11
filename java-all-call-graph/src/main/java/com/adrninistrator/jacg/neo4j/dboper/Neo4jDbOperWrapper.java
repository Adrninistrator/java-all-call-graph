package com.adrninistrator.jacg.neo4j.dboper;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.callgraph.CallGraphNode4Caller;
import com.adrninistrator.jacg.dto.method.FullMethodWithReturnType;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCall;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodLineNumber;
import com.adrninistrator.jacg.neo4j.common.Neo4jColumnConstants;
import com.adrninistrator.jacg.neo4j.repository.JACGClassNameRepository;
import com.adrninistrator.jacg.neo4j.repository.JACGMethodCallRepository;
import com.adrninistrator.jacg.neo4j.repository.JACGMethodInMCRepository;
import com.adrninistrator.jacg.neo4j.repository.JACGMethodInfoRepository;
import com.adrninistrator.jacg.neo4j.repository.JACGMethodLineNumberRepository;
import com.adrninistrator.jacg.neo4j.util.JACGNeo4jUtil;
import com.adrninistrator.jacg.spring.configuration.JACGSpringConfiguration;
import com.adrninistrator.jacg.spring.context.SpringContextManager;
import com.adrninistrator.javacg2.common.enums.JavaCG2YesNoEnum;
import org.neo4j.ogm.model.QueryStatistics;
import org.neo4j.ogm.model.Result;
import org.neo4j.ogm.session.Session;
import org.neo4j.ogm.session.SessionFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.transaction.support.TransactionTemplate;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2024/7/22
 * @description:
 */
public class Neo4jDbOperWrapper extends DbOperWrapper {

    private static final Logger logger = LoggerFactory.getLogger(Neo4jDbOperWrapper.class);

    private final TransactionTemplate transactionTemplate;
    private final SessionFactory sessionFactory;
    private final JACGClassNameRepository jacgClassNameRepository;
    private final JACGMethodInMCRepository jacgMethodInMCRepository;
    private final JACGMethodCallRepository jacgMethodCallRepository;
    private final JACGMethodInfoRepository jacgMethodInfoRepository;
    private final JACGMethodLineNumberRepository jacgMethodLineNumberRepository;

    public Neo4jDbOperWrapper(String appName, int dbInsertBatchSize) {
        this.appName = appName;
        this.dbInsertBatchSize = dbInsertBatchSize;
        tableSuffix = "";
        ApplicationContext applicationContext = SpringContextManager.getApplicationContext();
        transactionTemplate = applicationContext.getBean(TransactionTemplate.class, JACGSpringConfiguration.NEO4J_TRANSACTION_TEMPLATE_NAME);
        sessionFactory = applicationContext.getBean(SessionFactory.class, JACGSpringConfiguration.NEO4J_SESSION_FACTORY_NAME);
        jacgClassNameRepository = applicationContext.getBean(JACGClassNameRepository.class);
        jacgMethodInMCRepository = applicationContext.getBean(JACGMethodInMCRepository.class);
        jacgMethodCallRepository = applicationContext.getBean(JACGMethodCallRepository.class);
        jacgMethodInfoRepository = applicationContext.getBean(JACGMethodInfoRepository.class);
        jacgMethodLineNumberRepository = applicationContext.getBean(JACGMethodLineNumberRepository.class);

        objSeq = "dbwo4n@" + ATOMIC_INTEGER.incrementAndGet();
        logger.info("objSeq [{}]", objSeq);
    }

    private String getStringValue(Map<String, Object> map, String key) {
        return (String) map.get(key);
    }

    private Integer getIntValue(Map<String, Object> map, String key) {
        return ((Long) map.get(key)).intValue();
    }

    /**
     * 将类名表中的同名类更新为使用完整类名之前，查找类名相同但包名不同的类
     *
     * @return
     */
    @Override
    public Set<String> findDuplicateClassBeforeUpdate() {
        return jacgClassNameRepository.queryDupSimpleClassName(appName);
    }

    // 执行将简单类名更新为完整类名
    @Override
    protected boolean doUpdateSimpleClassName() {
        return Boolean.TRUE.equals(transactionTemplate.execute(status -> {
            for (String duplicateSimpleClassName : duplicateSimpleClassNameSet) {
                if (jacgClassNameRepository.updateSimpleClassName2Full(appName, duplicateSimpleClassName, JavaCG2YesNoEnum.YES.getIntValue()) <= 0) {
                    return false;
                }
            }
            return true;
        }));
    }

    // 执行查找类名与唯一类名相同的唯一类名
    @Override
    protected List<String> doFindDuplicateClass(String tableSuffix) {
        return jacgClassNameRepository.queryDupClassNameByFlag(appName, JavaCG2YesNoEnum.YES.getIntValue());
    }

    // 根据完整类名查询对应的唯一类名
    @Override
    protected String querySimpleClassNameByFull(String className) {
        return jacgClassNameRepository.querySimpleClassNameByFull(appName, className);
    }

    // 根据简单类名查询对应的唯一类名
    @Override
    protected String querySimpleClassNameBySimple(String simpleCassName) {
        return jacgClassNameRepository.querySimpleClassNameBySimple(appName, simpleCassName);
    }

    // 根据类名查询相关的方法
    @Override
    public List<FullMethodWithReturnType> queryMethodByClassName(String className) {
        List<WriteDbData4MethodInfo> list = jacgMethodInfoRepository.queryFullMethodWithAppName(appName, className);
        return genFullMethodWithReturnTypeList(list);
    }

    // 根据类名及完整方法前缀查询方法信息
    @Override
    public List<WriteDbData4MethodInfo> queryMethodInfoByClassMethodPrefix(String className, String fullMethodPrefix) {
        return jacgMethodInfoRepository.queryMethodInfoByClassFullMethodPrefix(appName, className, fullMethodPrefix);
    }

    // 根据调用方简单类名，查找1个对应的完整方法
    @Override
    public String queryOneFullMethodByCallerSCN(String callerSimpleClassName) {
        return jacgMethodInMCRepository.queryOneFullMethodByCallerSCN(appName, callerSimpleClassName);
    }

    // 通过方法名称获取调用方方法
    @Override
    public List<WriteDbData4MethodCall> queryCallerMethodByName(String callerSimpleClassName, String fullMethodPrefix) {
        List<Map<String, Object>> methodCallList = jacgMethodCallRepository.queryCallerMethodByName(appName, callerSimpleClassName, fullMethodPrefix);
        List<WriteDbData4MethodCall> list = new ArrayList<>(methodCallList.size());
        for (Map<String, Object> methodCallMap : methodCallList) {
            WriteDbData4MethodCall writeDbData4MethodCall = new WriteDbData4MethodCall();
            writeDbData4MethodCall.setCallerMethodHash(getStringValue(methodCallMap, Neo4jColumnConstants.MC_METHOD_HASH));
            writeDbData4MethodCall.setCallerFullMethod(getStringValue(methodCallMap, Neo4jColumnConstants.MC_FULL_METHOD));
            writeDbData4MethodCall.setCallerReturnType(getStringValue(methodCallMap, Neo4jColumnConstants.MC_RETURN_TYPE));
            writeDbData4MethodCall.setCallFlags(getIntValue(methodCallMap, Neo4jColumnConstants.MC_CALL_FLAGS));
            list.add(writeDbData4MethodCall);
        }
        return list;
    }

    // 查询当前节点的一个下层被调用方法
    @Override
    public WriteDbData4MethodCall queryOneCalleeMethod(CallGraphNode4Caller callGraphNode4Caller, int lineNumStart, int lineNumEnd) {
        boolean useLineNum = lineNumStart != JACGConstants.LINE_NUM_NONE && lineNumEnd != JACGConstants.LINE_NUM_NONE;
        Map<String, Object> methodCallMap;
        if (!useLineNum) {
            methodCallMap = jacgMethodCallRepository.queryOneCalleeMethod(appName, callGraphNode4Caller.getCallerMethodHash(), callGraphNode4Caller.getMethodCallId());
        } else {
            methodCallMap = jacgMethodCallRepository.queryOneCalleeMethodByLine(appName, callGraphNode4Caller.getCallerMethodHash(), callGraphNode4Caller.getMethodCallId(),
                    lineNumStart, lineNumEnd);
        }
        if (methodCallMap.isEmpty()) {
            return null;
        }
        WriteDbData4MethodCall writeDbData4MethodCall = new WriteDbData4MethodCall();
        writeDbData4MethodCall.setCallId(getIntValue(methodCallMap, Neo4jColumnConstants.MC_CALL_ID));
        writeDbData4MethodCall.setCallType(getStringValue(methodCallMap, Neo4jColumnConstants.MC_CALL_TYPE));
        writeDbData4MethodCall.setEnabled(getIntValue(methodCallMap, Neo4jColumnConstants.MC_CALL_ENABLED));
        writeDbData4MethodCall.setCalleeFullMethod(getStringValue(methodCallMap, Neo4jColumnConstants.MC_CALLEE_FULL_METHOD));
        writeDbData4MethodCall.setCalleeMethodHash(getStringValue(methodCallMap, Neo4jColumnConstants.MC_CALLEE_METHOD_HASH));
        writeDbData4MethodCall.setCallerLineNumber(getIntValue(methodCallMap, Neo4jColumnConstants.MC_CALLER_LINE_NUMBER));
        writeDbData4MethodCall.setCallFlags(getIntValue(methodCallMap, Neo4jColumnConstants.MC_CALL_FLAGS));
        writeDbData4MethodCall.setRawReturnType(getStringValue(methodCallMap, Neo4jColumnConstants.MC_RAW_RETURN_TYPE));
        writeDbData4MethodCall.setCallerReturnType(getStringValue(methodCallMap, Neo4jColumnConstants.MC_CALLER_RETURN_TYPE));
        return writeDbData4MethodCall;
    }

    @Override
    public String queryMethodHashByPrefix(String simpleClassName, String fullMethodPrefix, String returnType) {
        return jacgMethodInfoRepository.queryMethodHashByPrefix(appName, simpleClassName, fullMethodPrefix,  returnType);
    }

    @Override
    public WriteDbData4MethodLineNumber queryMethodLineNumber(String simpleClassName, int methodLineNum) {
        return jacgMethodLineNumberRepository.queryMethodLineNumber(appName, simpleClassName, methodLineNum);
    }

    @Override
    public WriteDbData4MethodCall queryMethodCallExtraInfo(boolean isCallee, String methodHash) {
        Map<String, Object> methodCallMap;
        if (isCallee) {
            methodCallMap = jacgMethodCallRepository.queryMethodCallExtraInfoByCallee(appName, methodHash);
        } else {
            methodCallMap = jacgMethodCallRepository.queryMethodCallExtraInfoByCaller(appName, methodHash);
        }
        WriteDbData4MethodCall writeDbData4MethodCall = new WriteDbData4MethodCall();
        writeDbData4MethodCall.setCallerReturnType(getStringValue(methodCallMap, Neo4jColumnConstants.MC_CALLER_RETURN_TYPE));
        writeDbData4MethodCall.setCallFlags(getIntValue(methodCallMap, Neo4jColumnConstants.MC_CALL_FLAGS));
        writeDbData4MethodCall.setRawReturnType(getStringValue(methodCallMap, Neo4jColumnConstants.MC_RAW_RETURN_TYPE));
        return writeDbData4MethodCall;
    }

    // 清理节点数据
    public boolean deleteNodeData(String nodeName) {
        Session session = sessionFactory.openSession();
        Map<String, String> map = new HashMap<>();
        map.put("appName", appName);
        Result result = session.query("MATCH (p:" + nodeName + " {appName:$appName}) DETACH DELETE p", map);
        QueryStatistics queryStatistics = result.queryStatistics();
        logger.info("清理appName为 {} 的 {} 节点，清理的节点数量: {} 清理的关系数量: {}", appName, nodeName, queryStatistics.getNodesDeleted(), queryStatistics.getRelationshipsDeleted());
        return true;
    }

    /**
     * 查询索引信息
     *
     * @param nodeName
     * @return
     */
    public List<Map<String, Object>> queryIndexInfo(String nodeName) {
        List<Map<String, Object>> list = new ArrayList<>();
        Map<String, String> parameterMap = new HashMap<>();
        parameterMap.put("nodeName", nodeName);
        Session session = sessionFactory.openSession();
        Result result = session.query("show indexes WHERE $nodeName in labelsOrTypes", parameterMap);
        Iterator<Map<String, Object>> iterator = result.iterator();
        iterator.forEachRemaining(list::add);
        return list;
    }

    // 创建索引
    public void createIndex(String indexName, String nodeName, String... properties) {
        StringBuilder propertiesSB = new StringBuilder();
        boolean first = true;
        for (String property : properties) {
            if (!first) {
                propertiesSB.append(", ");
            } else {
                first = false;
            }
            propertiesSB.append("n.").append(property);
        }
        String cypher = String.format("CREATE INDEX %s FOR (n:%s) ON (%s)", indexName, nodeName, propertiesSB);
        Session session = sessionFactory.openSession();
        Result result = session.query(cypher, Collections.emptyMap());
        int indexesAddedNum = result.queryStatistics().getIndexesAdded();
        logger.info("索引创建数量 {} {} {} {}", indexName, nodeName, JACGNeo4jUtil.formatPropertiesInIndex(properties), indexesAddedNum);
    }
}
