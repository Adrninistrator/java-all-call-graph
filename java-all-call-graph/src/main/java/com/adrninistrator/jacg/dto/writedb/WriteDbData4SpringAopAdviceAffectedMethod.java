package com.adrninistrator.jacg.dto.writedb;

import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData;

/**
 * @author adrninistrator
 * @date 2025/7/9
 * @description: 用于写入数据库的数据，Spring AOP advice影响的方法信息
 */
public class WriteDbData4SpringAopAdviceAffectedMethod implements BaseWriteDbData {

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
    private String underlyingExpression;
    private String affectedFullMethod;
    private String affectedMethodReturnType;
    private String affectedMethodHash;

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

    public String getUnderlyingExpression() {
        return underlyingExpression;
    }

    public void setUnderlyingExpression(String underlyingExpression) {
        this.underlyingExpression = underlyingExpression;
    }

    public String getAffectedFullMethod() {
        return affectedFullMethod;
    }

    public void setAffectedFullMethod(String affectedFullMethod) {
        this.affectedFullMethod = affectedFullMethod;
    }

    public String getAffectedMethodReturnType() {
        return affectedMethodReturnType;
    }

    public void setAffectedMethodReturnType(String affectedMethodReturnType) {
        this.affectedMethodReturnType = affectedMethodReturnType;
    }

    public String getAffectedMethodHash() {
        return affectedMethodHash;
    }

    public void setAffectedMethodHash(String affectedMethodHash) {
        this.affectedMethodHash = affectedMethodHash;
    }
}
