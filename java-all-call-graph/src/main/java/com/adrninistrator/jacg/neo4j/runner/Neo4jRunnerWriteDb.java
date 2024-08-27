package com.adrninistrator.jacg.neo4j.runner;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4ClassInfo;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4ClassName;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4ClassReference;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4ExtendsImpl;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4GetMethod;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4MethodCall;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4MethodInfo;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4MethodLineNumber;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4SetMethod;
import com.adrninistrator.jacg.neo4j.annoataion.Neo4jIndex;
import com.adrninistrator.jacg.neo4j.annoataion.Neo4jIndexes;
import com.adrninistrator.jacg.neo4j.dboper.Neo4jDbOperWrapper;
import com.adrninistrator.jacg.neo4j.domain.node.AbstractJACGNeo4jNode;
import com.adrninistrator.jacg.neo4j.util.JACGNeo4jUtil;
import com.adrninistrator.jacg.neo4j.writedb.Neo4jWriteDbHandler4ClassInfo;
import com.adrninistrator.jacg.neo4j.writedb.Neo4jWriteDbHandler4ClassName;
import com.adrninistrator.jacg.neo4j.writedb.Neo4jWriteDbHandler4ClassReference;
import com.adrninistrator.jacg.neo4j.writedb.Neo4jWriteDbHandler4ExtendsImpl;
import com.adrninistrator.jacg.neo4j.writedb.Neo4jWriteDbHandler4GetMethod;
import com.adrninistrator.jacg.neo4j.writedb.Neo4jWriteDbHandler4MethodCall;
import com.adrninistrator.jacg.neo4j.writedb.Neo4jWriteDbHandler4MethodInfo;
import com.adrninistrator.jacg.neo4j.writedb.Neo4jWriteDbHandler4MethodLineNumber;
import com.adrninistrator.jacg.neo4j.writedb.Neo4jWriteDbHandler4SetMethod;
import com.adrninistrator.jacg.runner.RunnerWriteDb;
import com.adrninistrator.jacg.util.JACGFindClassUtil;
import com.adrninistrator.javacg.dto.counter.JavaCGCounter;
import com.adrninistrator.javacg.exceptions.JavaCGRuntimeException;
import com.adrninistrator.javacg.util.JavaCGUtil;
import org.neo4j.ogm.annotation.NodeEntity;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2024/7/21
 * @description: 生成Java方法调用关系文件并写入neo4j
 */
public class Neo4jRunnerWriteDb extends RunnerWriteDb {
    private static final Logger logger = LoggerFactory.getLogger(Neo4jRunnerWriteDb.class);

    private final Neo4jDbOperWrapper neo4jDbOperWrapper;

    private final List<String> nodeClassNameList;

    // 写数据库的标志，为true时在将数据写入Neo4j后，还会写入数据库中（默认不写入数据库）
    private boolean writeDbFlag = false;

    public Neo4jRunnerWriteDb(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
        neo4jDbOperWrapper = (Neo4jDbOperWrapper) dbOperWrapper;

        nodeClassNameList = JACGFindClassUtil.getOrdinaryClassNameListFromDirOrJar(AbstractJACGNeo4jNode.class);
        if (JavaCGUtil.isCollectionEmpty(nodeClassNameList)) {
            logger.error("未找到neo4j节点类");
            throw new JavaCGRuntimeException("未找到neo4j节点类");
        }
    }

    @Override
    protected boolean handleDb() {
        return false;
    }

    @Override
    protected boolean useNeo4j() {
        return true;
    }

    @Override
    protected WriteDbHandler4ClassName genWriteDbHandler4ClassName() {
        return new Neo4jWriteDbHandler4ClassName(writeDbResult);
    }

    @Override
    protected WriteDbHandler4ClassReference genWriteDbHandler4ClassReference() {
        return new Neo4jWriteDbHandler4ClassReference(writeDbResult);
    }

    @Override
    protected WriteDbHandler4ExtendsImpl genWriteDbHandler4ExtendsImpl() {
        return new Neo4jWriteDbHandler4ExtendsImpl(writeDbResult);
    }

    @Override
    protected WriteDbHandler4MethodCall genWriteDbHandler4MethodCall() {
        return new Neo4jWriteDbHandler4MethodCall(writeDbResult);
    }

    @Override
    protected WriteDbHandler4ClassInfo genWriteDbHandler4ClassInfo() {
        return new Neo4jWriteDbHandler4ClassInfo(writeDbResult);
    }

    @Override
    protected WriteDbHandler4MethodInfo genWriteDbHandler4MethodInfo() {
        return new Neo4jWriteDbHandler4MethodInfo(writeDbResult);
    }

    @Override
    protected WriteDbHandler4MethodLineNumber genWriteDbHandler4MethodLineNumber() {
        return new Neo4jWriteDbHandler4MethodLineNumber(writeDbResult);
    }

    @Override
    protected WriteDbHandler4GetMethod genWriteDbHandler4GetMethod() {
        return new Neo4jWriteDbHandler4GetMethod(writeDbResult);
    }

    @Override
    protected WriteDbHandler4SetMethod genWriteDbHandler4SetMethod() {
        return new Neo4jWriteDbHandler4SetMethod(writeDbResult);
    }

    @Override
    protected boolean truncateTables() {
        logger.info("清理neo4j节点存量的appName相同的数据");

        try {
            for (String nodeClassName : nodeClassNameList) {
                Class<?> clazz = Class.forName(nodeClassName);
                NodeEntity nodeEntity = clazz.getAnnotation(NodeEntity.class);
                if (nodeEntity == null) {
                    continue;
                }
                String nodeName = nodeEntity.label();
                neo4jDbOperWrapper.deleteNodeData(nodeName);
            }
            return true;
        } catch (Exception e) {
            logger.error("出现异常 ", e);
            return false;
        }
    }

    // 创建索引
    private void createIndexes() {
        try {
            for (String nodeClassName : nodeClassNameList) {
                Class<?> clazz = Class.forName(nodeClassName);
                NodeEntity nodeEntity = clazz.getAnnotation(NodeEntity.class);
                if (nodeEntity == null) {
                    continue;
                }
                Neo4jIndexes neo4jIndexes = clazz.getAnnotation(Neo4jIndexes.class);
                if (neo4jIndexes == null) {
                    continue;
                }
                String nodeName = nodeEntity.label();
                List<Map<String, Object>> indexesList = neo4jDbOperWrapper.queryIndexInfo(nodeName);
                JavaCGCounter indexMaxSeq = new JavaCGCounter(0);
                Set<String> propertiesSet = new HashSet<>();
                // 遍历节点的索引并记录相关信息
                recordIndexInfo(indexesList, indexMaxSeq, propertiesSet);

                for (Neo4jIndex neo4jIndex : neo4jIndexes.indexes()) {
                    String propertiesInIndex = JACGNeo4jUtil.formatPropertiesInIndex(neo4jIndex.properties());
                    if (!propertiesSet.add(propertiesInIndex)) {
                        logger.info("当前节点属性对应的索引已存在，不再创建 {} [{}]", nodeName, propertiesInIndex);
                        continue;
                    }
                    // 创建对应索引
                    String indexName = JACGNeo4jUtil.genIndexName(nodeName, indexMaxSeq.addAndGet());
                    neo4jDbOperWrapper.createIndex(indexName, nodeName, neo4jIndex.properties());
                }
            }
        } catch (Exception e) {
            logger.error("出现异常 ", e);
            throw new JavaCGRuntimeException("创建索引失败");
        }
    }

    // 遍历节点的索引并记录相关信息
    private void recordIndexInfo(List<Map<String, Object>> indexesList, JavaCGCounter indexMaxSeq, Set<String> propertiesSet) {
        for (Map<String, Object> indexMap : indexesList) {
            String indexName = (String) indexMap.get("name");
            // 记录当前节点的索引的最大序号
            Integer indexSeq = JACGNeo4jUtil.getIndexSeq(indexName);
            if (indexSeq != null && indexSeq > indexMaxSeq.getCount()) {
                indexMaxSeq.setCount(indexSeq);
            }

            // 记录当前索引包含的属性
            String[] properties = (String[]) indexMap.get("properties");
            propertiesSet.add(JACGNeo4jUtil.formatPropertiesInIndex(properties));
        }
    }

    @Override
    public void handle() {
        super.handle();

        // 创建索引
        createIndexes();

        if (!writeDbFlag) {
            return;
        }
        // 将数据写入数据库
        RunnerWriteDb runnerWriteDb = new RunnerWriteDb(configureWrapper);
        if (!runnerWriteDb.run()) {
            this.setSomeTaskFail(true);
        }
        if (runnerWriteDb.isSomeTaskFail()) {
            this.setSomeTaskFail(true);
        }
    }

    public void setWriteDbFlag(boolean writeDbFlag) {
        this.writeDbFlag = writeDbFlag;
    }
}
