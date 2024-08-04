package com.adrninistrator.jacg.neo4j.domain.node;

import com.adrninistrator.jacg.neo4j.annoataion.Neo4jIndex;
import com.adrninistrator.jacg.neo4j.annoataion.Neo4jIndexes;
import com.adrninistrator.jacg.neo4j.idstrategy.JACGIdStrategy;
import org.neo4j.ogm.annotation.GeneratedValue;
import org.neo4j.ogm.annotation.Id;
import org.neo4j.ogm.annotation.NodeEntity;

/**
 * @author adrninistrator
 * @date 2024/7/21
 * @description:
 */
@NodeEntity(label = "jacg_method_in_method_call")
@Neo4jIndexes(indexes = {
        @Neo4jIndex(properties = {"appName", "simpleClassName"}),
        @Neo4jIndex(properties = {"appName", "methodHash"})
})
public class JACGMethodInMC {
    @Id
    @GeneratedValue(strategy = JACGIdStrategy.class)
    private String id;
    private String appName;
    private String methodHash;
    private String simpleClassName;
    private String methodName;
    private String fullMethod;
    private String returnType;
    private Integer jarNum;

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getAppName() {
        return appName;
    }

    public void setAppName(String appName) {
        this.appName = appName;
    }

    public String getMethodHash() {
        return methodHash;
    }

    public void setMethodHash(String methodHash) {
        this.methodHash = methodHash;
    }

    public String getSimpleClassName() {
        return simpleClassName;
    }

    public void setSimpleClassName(String simpleClassName) {
        this.simpleClassName = simpleClassName;
    }

    public String getMethodName() {
        return methodName;
    }

    public void setMethodName(String methodName) {
        this.methodName = methodName;
    }

    public String getFullMethod() {
        return fullMethod;
    }

    public void setFullMethod(String fullMethod) {
        this.fullMethod = fullMethod;
    }

    public String getReturnType() {
        return returnType;
    }

    public void setReturnType(String returnType) {
        this.returnType = returnType;
    }

    public Integer getJarNum() {
        return jarNum;
    }

    public void setJarNum(Integer jarNum) {
        this.jarNum = jarNum;
    }
}
