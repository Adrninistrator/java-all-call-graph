package com.adrninistrator.jacg.dto.writedb;

import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData;

/**
 * @author adrninistrator
 * @date 2025/6/29
 * @description: 用于写入数据库的数据，Spring AOP advice信息
 */
public class WriteDbData4SpringAopAdvice implements BaseWriteDbData {

    private int recordId;
    private String type;
    private String xmlAspectId;
    private String xmlAspectMethodName;
    private String adviceType;
    private String xmlPointcutRef;
    private String expression;
    private int aspectOrder;
    private String adviceFullMethod;
    private String adviceMethodReturnType;
    private String adviceMethodHash;
    private String aspectClassName;
    private String defineXmlPath;

    public int getRecordId() {
        return recordId;
    }

    public void setRecordId(int recordId) {
        this.recordId = recordId;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public String getXmlAspectId() {
        return xmlAspectId;
    }

    public void setXmlAspectId(String xmlAspectId) {
        this.xmlAspectId = xmlAspectId;
    }

    public String getXmlAspectMethodName() {
        return xmlAspectMethodName;
    }

    public void setXmlAspectMethodName(String xmlAspectMethodName) {
        this.xmlAspectMethodName = xmlAspectMethodName;
    }

    public String getAdviceType() {
        return adviceType;
    }

    public void setAdviceType(String adviceType) {
        this.adviceType = adviceType;
    }

    public String getXmlPointcutRef() {
        return xmlPointcutRef;
    }

    public void setXmlPointcutRef(String xmlPointcutRef) {
        this.xmlPointcutRef = xmlPointcutRef;
    }

    public String getExpression() {
        return expression;
    }

    public void setExpression(String expression) {
        this.expression = expression;
    }

    public int getAspectOrder() {
        return aspectOrder;
    }

    public void setAspectOrder(int aspectOrder) {
        this.aspectOrder = aspectOrder;
    }

    public String getAdviceFullMethod() {
        return adviceFullMethod;
    }

    public void setAdviceFullMethod(String adviceFullMethod) {
        this.adviceFullMethod = adviceFullMethod;
    }

    public String getAdviceMethodReturnType() {
        return adviceMethodReturnType;
    }

    public void setAdviceMethodReturnType(String adviceMethodReturnType) {
        this.adviceMethodReturnType = adviceMethodReturnType;
    }

    public String getAdviceMethodHash() {
        return adviceMethodHash;
    }

    public void setAdviceMethodHash(String adviceMethodHash) {
        this.adviceMethodHash = adviceMethodHash;
    }

    public String getAspectClassName() {
        return aspectClassName;
    }

    public void setAspectClassName(String aspectClassName) {
        this.aspectClassName = aspectClassName;
    }

    public String getDefineXmlPath() {
        return defineXmlPath;
    }

    public void setDefineXmlPath(String defineXmlPath) {
        this.defineXmlPath = defineXmlPath;
    }
}
