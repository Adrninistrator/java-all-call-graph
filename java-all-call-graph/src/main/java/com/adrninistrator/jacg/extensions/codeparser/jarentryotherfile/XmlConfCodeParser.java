package com.adrninistrator.jacg.extensions.codeparser.jarentryotherfile;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.util.JACGXmlUtil;
import com.adrninistrator.javacg2.common.enums.JavaCG2YesNoEnum;
import com.adrninistrator.javacg2.extensions.codeparser.AbstractSaveData2FileParser;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.jdom2.Element;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.io.InputStream;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2026/1/21
 * @description: 解析XML文件中的所有元素和属性
 */
public class XmlConfCodeParser extends AbstractSaveData2FileParser {
    private static final Logger logger = LoggerFactory.getLogger(XmlConfCodeParser.class);

    public static final String FILE_NAME = "xml_conf";

    // 类型常量
    public static final String TYPE_ELEMENT = "e";
    public static final String TYPE_ELEMENT_VALUE = "ev";
    public static final String TYPE_ELEMENT_ATTRIBUTE_VALUE = "eav";

    private int currentElementSeq = 0;
    private int currentXmlFileSeq = 0;
    private String lastXmlFilePath = "";

    @Override
    public String chooseFileName() {
        return FILE_NAME;
    }

    @Override
    public String[] chooseJarEntryOtherFileExt() {
        return new String[]{JACGConstants.EXT_XML};
    }

    @Override
    public boolean parseJarEntryOtherFile(InputStream inputStream, String jarEntryPath, String jarEntryName) {
        logger.info("处理{}文件 {}", JACGConstants.EXT_XML, jarEntryPath);

        // 通过XML文件路径判断，每找到一个新的，则XML文件序号加1，元素序号重置为0
        if (!jarEntryPath.equals(lastXmlFilePath)) {
            currentXmlFileSeq++;
            currentElementSeq = 0;
            lastXmlFilePath = jarEntryPath;
        }

        Element root;
        try {
            root = JACGXmlUtil.parseXmlRootElement(inputStream);
        } catch (Exception e) {
            logger.warn("解析XML文件出错 {} {}", jarEntryPath, e.getMessage());
            return true;
        }

        try {
            // 从根元素开始递归解析，根元素的父元素序号为null
            parseElement(root, jarEntryPath, jarEntryName, "", null, currentXmlFileSeq);
            return true;
        } catch (Exception e) {
            logger.error("error ", e);
            return false;
        }
    }

    /**
     * 递归解析XML元素
     * 深度优先方式：首先处理完当前元素本身、元素的值，及元素的属性值后，再开始处理子元素
     *
     * @param element          当前元素
     * @param jarEntryPath     XML文件路径
     * @param jarEntryName     XML文件名
     * @param parentNestedName 父元素的嵌套名称
     * @param parentSeq        父元素的序号
     * @param xmlFileSeq       XML文件序号
     * @throws IOException
     */
    private void parseElement(Element element, String jarEntryPath, String jarEntryName, String parentNestedName, Integer parentSeq, int xmlFileSeq) throws IOException {
        String elementName = element.getName();
        String nestedElementName;
        if (parentNestedName.isEmpty()) {
            nestedElementName = elementName;
        } else {
            nestedElementName = parentNestedName + "." + elementName;
        }

        // 生成当前元素的序号，从1开始累加
        currentElementSeq++;
        int currentElementSeqValue = currentElementSeq;

        // 获取元素值
        String elementValue = element.getTextTrim();

        boolean hasValue = !elementValue.isEmpty();
        boolean hasAttributes = !element.getAttributes().isEmpty();
        boolean hasChildren = !element.getChildren().isEmpty();

        // 记录元素本身（类型为"e"），无论是否有值、属性或子元素
        // 元素本身：元素序号为当前元素的序号，父元素序号为父元素序号，所在元素序号为空字符串
        writeData2File(jarEntryPath, jarEntryName, String.valueOf(xmlFileSeq),
                String.valueOf(currentElementSeqValue),
                parentSeq == null ? "" : String.valueOf(parentSeq),
                "",
                TYPE_ELEMENT, nestedElementName, elementName, "", JavaCG2YesNoEnum.NO.getStrValue(), "");

        // 如果元素有值，记录元素值（类型为"ev"）
        // 元素值：元素序号为空字符串，父元素序号为空字符串，所在元素序号为当前元素序号
        if (hasValue) {
            JavaCG2YesNoEnum useBase64 = JavaCG2YesNoEnum.NO;
            String valueToWrite = elementValue;
            if (JavaCG2Util.checkNeedBase64(elementValue)) {
                valueToWrite = JavaCG2Util.base64Encode(elementValue);
                useBase64 = JavaCG2YesNoEnum.YES;
            }
            writeData2File(jarEntryPath, jarEntryName, String.valueOf(xmlFileSeq),
                    "",
                    "",
                    String.valueOf(currentElementSeqValue),
                    TYPE_ELEMENT_VALUE, nestedElementName, elementName, "", useBase64.getStrValue(), valueToWrite);
        }

        // 记录元素的所有属性（类型为"eav"）
        // 元素属性值：元素序号为空字符串，父元素序号为空字符串，所在元素序号为当前元素序号
        if (hasAttributes) {
            List<org.jdom2.Attribute> attributes = element.getAttributes();
            for (org.jdom2.Attribute attr : attributes) {
                String attrName = attr.getName();
                String attrValue = attr.getValue();
                JavaCG2YesNoEnum useBase64 = JavaCG2YesNoEnum.NO;
                String valueToWrite = attrValue;
                if (JavaCG2Util.checkNeedBase64(attrValue)) {
                    valueToWrite = JavaCG2Util.base64Encode(attrValue);
                    useBase64 = JavaCG2YesNoEnum.YES;
                }
                writeData2File(jarEntryPath, jarEntryName, String.valueOf(xmlFileSeq),
                        "",
                        "",
                        String.valueOf(currentElementSeqValue),
                        TYPE_ELEMENT_ATTRIBUTE_VALUE, nestedElementName, elementName, attrName, useBase64.getStrValue(),
                        valueToWrite);
            }
        }

        // 递归处理子元素
        if (hasChildren) {
            List<Element> children = element.getChildren();
            for (Element child : children) {
                parseElement(child, jarEntryPath, jarEntryName, nestedElementName, currentElementSeqValue, xmlFileSeq);
            }
        }
    }
}
