package com.adrninistrator.jacg.dto.writedb;

import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData;

/**
 * @author adrninistrator
 * @date 2025/6/29
 * @description: 用于写入数据库的数据，Spring AOP aspect信息
 */
public class WriteDbData4SpringAopAspect implements BaseWriteDbData {

    private int recordId;
    private String type;
    private String xmlAspectId;
    private String xmlAspectRef;
    private int aspectOrder;
    private String className;
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

    public String getXmlAspectRef() {
        return xmlAspectRef;
    }

    public void setXmlAspectRef(String xmlAspectRef) {
        this.xmlAspectRef = xmlAspectRef;
    }

    public int getAspectOrder() {
        return aspectOrder;
    }

    public void setAspectOrder(int aspectOrder) {
        this.aspectOrder = aspectOrder;
    }

    public String getClassName() {
        return className;
    }

    public void setClassName(String className) {
        this.className = className;
    }

    public String getDefineXmlPath() {
        return defineXmlPath;
    }

    public void setDefineXmlPath(String defineXmlPath) {
        this.defineXmlPath = defineXmlPath;
    }
}
