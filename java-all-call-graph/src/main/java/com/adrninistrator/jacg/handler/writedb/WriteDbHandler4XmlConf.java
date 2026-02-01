package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4XmlConf;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.extensions.codeparser.jarentryotherfile.XmlConfCodeParser;
import com.adrninistrator.javacg2.common.enums.JavaCG2YesNoEnum;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.apache.commons.lang3.StringUtils;

/**
 * @author adrninistrator
 * @date 2026/1/21
 * @description: 写入数据库，XML文件配置内容
 */
@JACGWriteDbHandler(
        readFile = true,
        otherFileName = XmlConfCodeParser.FILE_NAME,
        minColumnNum = 12,
        maxColumnNum = 12,
        // 读取文件时允许最后一列出现TAB
        allowTabInLastColumn = true,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_XML_CONF
)
public class WriteDbHandler4XmlConf extends AbstractWriteDbHandler<WriteDbData4XmlConf> {

    public WriteDbHandler4XmlConf(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4XmlConf genData(String[] array) {
        String xmlFilePath = readLineData();
        String xmlFileName = readLineData();
        String xmlFileSeqStr = readLineData();
        String elementSeqStr = readLineData();
        String parentSeqStr = readLineData();
        String inElementSeqStr = readLineData();
        String type = readLineData();
        String nestedElementName = readLineData();
        String elementName = readLineData();
        String attributeName = readLineData();
        boolean useBase64 = JavaCG2YesNoEnum.isYes(readLineData());
        String elementValue = readLineData();

        if (useBase64) {
            elementValue = JavaCG2Util.base64Decode(elementValue);
        }

        WriteDbData4XmlConf writeDbData4XmlConf = new WriteDbData4XmlConf();
        // 记录ID在genObjectArray方法中调用genNextRecordId()方法获得，此处设置为0占位
        writeDbData4XmlConf.setRecordId(0);
        writeDbData4XmlConf.setXmlFilePath(xmlFilePath);
        writeDbData4XmlConf.setXmlFileName(xmlFileName);
        writeDbData4XmlConf.setXmlFileSeq(Integer.parseInt(xmlFileSeqStr));
        // 元素序号为空字符串时转换为null
        writeDbData4XmlConf.setElementSeq(StringUtils.isBlank(elementSeqStr) ? null : Integer.parseInt(elementSeqStr));
        // 父元素序号为空字符串时转换为null
        writeDbData4XmlConf.setParentSeq(StringUtils.isBlank(parentSeqStr) ? null : Integer.parseInt(parentSeqStr));
        // 所在元素序号为空字符串时转换为null
        writeDbData4XmlConf.setInElementSeq(StringUtils.isBlank(inElementSeqStr) ? null : Integer.parseInt(inElementSeqStr));
        writeDbData4XmlConf.setType(type);
        writeDbData4XmlConf.setNestedElementName(nestedElementName);
        writeDbData4XmlConf.setElementName(elementName);
        writeDbData4XmlConf.setAttributeName(StringUtils.isBlank(attributeName) ? null : attributeName);
        writeDbData4XmlConf.setElementValue(elementValue);
        return writeDbData4XmlConf;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4XmlConf data) {
        return new Object[]{
                genNextRecordId(),
                data.getXmlFilePath(),
                data.getXmlFileName(),
                data.getXmlFileSeq(),
                data.getElementSeq(),
                data.getParentSeq(),
                data.getInElementSeq(),
                data.getType(),
                data.getNestedElementName(),
                data.getElementName(),
                data.getAttributeName(),
                data.getElementValue()
        };
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "XML配置文件路径",
                "XML文件名",
                "XML文件序号",
                "元素序号",
                "父元素序号",
                "所在元素序号",
                "类型（e:元素，ev:元素值，eav:元素属性值）",
                "嵌套的元素名称",
                "当前的元素名称",
                "当前的元素属性名称",
                "元素值是否经过BASE64编码，1:是，0:否",
                "当前的元素值或元素属性值"
        };
    }

    @Override
    public String chooseNotMainFileDesc() {
        return "XML文件配置内容";
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{"保存XML文件名，及其中的元素和属性"};
    }
}
