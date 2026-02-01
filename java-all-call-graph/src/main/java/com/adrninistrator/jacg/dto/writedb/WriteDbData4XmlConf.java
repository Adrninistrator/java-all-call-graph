package com.adrninistrator.jacg.dto.writedb;

import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData;

/**
 * @author adrninistrator
 * @date 2026/1/21
 * @description: 用于写入数据库的数据，XML文件配置内容
 */
public class WriteDbData4XmlConf implements BaseWriteDbData {

    private int recordId;
    private String xmlFilePath;
    private String xmlFileName;
    private int xmlFileSeq;
    private Integer elementSeq;
    private Integer parentSeq;
    private Integer inElementSeq;
    private String type;
    private String nestedElementName;
    private String elementName;
    private String attributeName;
    private String elementValue;

    public int getRecordId() {
        return recordId;
    }

    public void setRecordId(int recordId) {
        this.recordId = recordId;
    }

    public String getXmlFilePath() {
        return xmlFilePath;
    }

    public void setXmlFilePath(String xmlFilePath) {
        this.xmlFilePath = xmlFilePath;
    }

    public String getXmlFileName() {
        return xmlFileName;
    }

    public void setXmlFileName(String xmlFileName) {
        this.xmlFileName = xmlFileName;
    }

    public int getXmlFileSeq() {
        return xmlFileSeq;
    }

    public void setXmlFileSeq(int xmlFileSeq) {
        this.xmlFileSeq = xmlFileSeq;
    }

    public Integer getElementSeq() {
        return elementSeq;
    }

    public void setElementSeq(Integer elementSeq) {
        this.elementSeq = elementSeq;
    }

    public Integer getParentSeq() {
        return parentSeq;
    }

    public void setParentSeq(Integer parentSeq) {
        this.parentSeq = parentSeq;
    }

    public Integer getInElementSeq() {
        return inElementSeq;
    }

    public void setInElementSeq(Integer inElementSeq) {
        this.inElementSeq = inElementSeq;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public String getNestedElementName() {
        return nestedElementName;
    }

    public void setNestedElementName(String nestedElementName) {
        this.nestedElementName = nestedElementName;
    }

    public String getElementName() {
        return elementName;
    }

    public void setElementName(String elementName) {
        this.elementName = elementName;
    }

    public String getAttributeName() {
        return attributeName;
    }

    public void setAttributeName(String attributeName) {
        this.attributeName = attributeName;
    }

    public String getElementValue() {
        return elementValue;
    }

    public void setElementValue(String elementValue) {
        this.elementValue = elementValue;
    }
}
