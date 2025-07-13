package com.adrninistrator.jacg.dto.writedb;

import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData;

/**
 * @author adrninistrator
 * @date 2025/6/29
 * @description: 用于写入数据库的数据，Spring AOP pointcut信息
 */
public class WriteDbData4SpringAopPointcut implements BaseWriteDbData {

    private int recordId;
    private String type;
    private String xmlPointcutId;
    private String expression;
    private String fullMethod;
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

    public String getXmlPointcutId() {
        return xmlPointcutId;
    }

    public void setXmlPointcutId(String xmlPointcutId) {
        this.xmlPointcutId = xmlPointcutId;
    }

    public String getExpression() {
        return expression;
    }

    public void setExpression(String expression) {
        this.expression = expression;
    }

    public String getFullMethod() {
        return fullMethod;
    }

    public void setFullMethod(String fullMethod) {
        this.fullMethod = fullMethod;
    }

    public String getDefineXmlPath() {
        return defineXmlPath;
    }

    public void setDefineXmlPath(String defineXmlPath) {
        this.defineXmlPath = defineXmlPath;
    }
}
