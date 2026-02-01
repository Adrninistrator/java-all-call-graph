package com.adrninistrator.jacg.handler.conf;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4XmlConf;
import com.adrninistrator.jacg.extensions.codeparser.jarentryotherfile.XmlConfCodeParser;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.handler.dto.XmlConfWithProperties;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import org.apache.commons.lang3.StringUtils;

import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author adrninistrator
 * @date 2026/1/23
 * @description: 查询XML配置文件的类
 */
public class XmlConfHandler extends BaseHandler {

    /**
     * Properties配置文件处理器
     */
    private final PropertiesConfHandler propertiesConfHandler;

    /**
     * 匹配 ${xxx} 格式的正则表达式
     */
    private static final Pattern PROPERTY_REF_PATTERN = Pattern.compile("\\$\\{([^}]+)\\}");

    /**
     * 匹配 ${a:b} 格式的正则表达式
     */
    private static final Pattern PROPERTY_REF_PATTERN_WITH_DEFAULT = Pattern.compile("\\$\\{([^:]+):([^}]+)\\}");

    public XmlConfHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
        propertiesConfHandler = new PropertiesConfHandler(dbOperWrapper);
    }

    public XmlConfHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
        propertiesConfHandler = new PropertiesConfHandler(dbOperWrapper);
    }

    /**
     * 根据XML嵌套的元素名称、元素名称、属性名称及属性值查询XML配置
     * 查询根据XML嵌套的元素名称、元素名称、属性名称及属性值都匹配的，对应表的全部字段
     *
     * @param nestedElementName XML嵌套的元素名称
     * @param elementName       XML元素名称
     * @param attributeName     属性名称
     * @param attributeValue    属性值
     * @return 匹配的XML配置数据列表
     */
    public List<WriteDbData4XmlConf> queryXmlConfByElementAttribute(String nestedElementName, String elementName,
                                                                    String attributeName, String attributeValue) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.XC_QUERY_BY_ELEMENT_ATTRIBUTE;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_XML_CONF) +
                    " from " + DbTableInfoEnum.DTIE_XML_CONF.getTableName() +
                    " where " + DC.XC_NESTED_ELEMENT_NAME + " = ?" +
                    " and " + DC.XC_ELEMENT_NAME + " = ?" +
                    " and " + DC.XC_ATTRIBUTE_NAME + " = ?" +
                    " and " + DC.XC_ELEMENT_VALUE + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        return dbOperator.queryList(sql, WriteDbData4XmlConf.class, nestedElementName, elementName, attributeName, attributeValue);
    }

    /**
     * 根据元素序号查询名称匹配的子元素
     * 子元素的parent_seq等于父元素的element_seq，且type为'e'（元素），可选择根据元素名称过滤
     *
     * @param xmlFileSeq  XML文件序号
     * @param elementSeq   元素序号
     * @param elementName  可选的元素名称，用于过滤，为null时不过滤
     * @return 子元素列表
     */
    public List<WriteDbData4XmlConf> queryChildrenByElementSeq(int xmlFileSeq, int elementSeq, String elementName) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.XC_QUERY_CHILDREN_BY_ELEMENT_SEQ;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_XML_CONF) +
                    " from " + DbTableInfoEnum.DTIE_XML_CONF.getTableName() +
                    " where " + DC.XC_XML_FILE_SEQ + " = ?" +
                    " and " + DC.XC_PARENT_SEQ + " = ?" +
                    " and " + DC.XC_TYPE + " = ?";
            if (StringUtils.isNotBlank(elementName)) {
                sql = sql + " and " + DC.XC_ELEMENT_NAME + " = ?";
            }
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        if (StringUtils.isNotBlank(elementName)) {
            return dbOperator.queryList(sql, WriteDbData4XmlConf.class, xmlFileSeq, elementSeq, XmlConfCodeParser.TYPE_ELEMENT, elementName);
        } else {
            return dbOperator.queryList(sql, WriteDbData4XmlConf.class, xmlFileSeq, elementSeq, XmlConfCodeParser.TYPE_ELEMENT);
        }
    }

    /**
     * 判断元素的属性名称与属性值是否有匹配的记录
     *
     * @param xmlFileSeq   XML文件序号
     * @param elementSeq    属性所在元素序号
     * @param attributeName  元素的属性名称
     * @param attributeValue 元素的属性值
     * @return 是否存在匹配的记录
     */
    public boolean checkAttributeExists(int xmlFileSeq, int elementSeq, String attributeName, String attributeValue) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.XC_CHECK_ATTRIBUTE_EXISTS;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select count(*) from " + DbTableInfoEnum.DTIE_XML_CONF.getTableName() +
                    " where " + DC.XC_XML_FILE_SEQ + " = ?" +
                    " and " + DC.XC_IN_ELEMENT_SEQ + " = ?" +
                    " and " + DC.XC_TYPE + " = ?" +
                    " and " + DC.XC_ATTRIBUTE_NAME + " = ?" +
                    " and " + DC.XC_ELEMENT_VALUE + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        Integer count = dbOperator.queryObjectOneColumn(sql, Integer.class, xmlFileSeq, elementSeq,
                XmlConfCodeParser.TYPE_ELEMENT_ATTRIBUTE_VALUE, attributeName, attributeValue);
        return count != null && count > 0;
    }

    /**
     * 根据元素的属性名称获取属性值
     *
     * @param xmlFileSeq   XML文件序号
     * @param elementSeq    属性所在元素序号
     * @param attributeName 元素的属性名称
     * @return 属性值，不存在时返回null
     */
    public String queryAttributeValue(int xmlFileSeq, int elementSeq, String attributeName) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.XC_QUERY_ATTRIBUTE_VALUE;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + DC.XC_ELEMENT_VALUE +
                    " from " + DbTableInfoEnum.DTIE_XML_CONF.getTableName() +
                    " where " + DC.XC_XML_FILE_SEQ + " = ?" +
                    " and " + DC.XC_IN_ELEMENT_SEQ + " = ?" +
                    " and " + DC.XC_TYPE + " = ?" +
                    " and " + DC.XC_ATTRIBUTE_NAME + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        return dbOperator.queryObjectOneColumn(sql, String.class, xmlFileSeq, elementSeq,
                XmlConfCodeParser.TYPE_ELEMENT_ATTRIBUTE_VALUE, attributeName);
    }

    /**
     * 根据元素的属性名称获取属性值，支持从.properties文件获取
     *
     * @param xmlFileSeq   XML文件序号
     * @param elementSeq    属性所在元素序号
     * @param attributeName 元素的属性名称
     * @return XML配置信息，包含原始值、默认值（如果有）和从properties文件查询到的记录
     */
    public XmlConfWithProperties queryAttributeValueWithProperties(int xmlFileSeq, int elementSeq, String attributeName) {
        // 调用queryAttributeValue方法获取原始的元素属性值
        // 入参包括：XML文件序号、属性所在元素序号、元素的属性名称
        // 返回String类型的属性值
        String originalValue = queryAttributeValue(xmlFileSeq, elementSeq, attributeName);

        XmlConfWithProperties xmlConfWithProperties = new XmlConfWithProperties();
        xmlConfWithProperties.setOriginalValue(originalValue);

        // 假如查询到的元素属性值为空，则直接返回
        if (StringUtils.isBlank(originalValue)) {
            return xmlConfWithProperties;
        }

        // 非"${}"形式
        // 假如查询到的元素属性值不是"${}"形式，则原始的元素属性值使用对应值，直接返回
        Matcher matcher = PROPERTY_REF_PATTERN.matcher(originalValue);
        if (!matcher.matches()) {
            return xmlConfWithProperties;
        }

        // "${}"形式或"${a:b}"形式
        // 匹配 ${a:b} 格式
        Matcher matcherWithDefault = PROPERTY_REF_PATTERN_WITH_DEFAULT.matcher(originalValue);
        if (matcherWithDefault.matches()) {
            // "${a:b}"形式
            String propKey = matcherWithDefault.group(1);
            String defaultValue = matcherWithDefault.group(2);

            // 元素属性值对应的默认值使用以上"b"的值
            xmlConfWithProperties.setDefaultValue(defaultValue);

            // 调用PropertiesConfHandler类对应方法查询对应列表
            // properties的名称使用以上"a"的值，保存到XmlConfWithProperties对应字段中
            List<com.adrninistrator.jacg.dto.writedb.WriteDbData4PropertiesConf> propConfList =
                    propertiesConfHandler.queryPropConfList(propKey, false);
            xmlConfWithProperties.setPropertiesConfList(propConfList);
        } else {
            // "${}"形式（不是"${a:b}"形式）
            String propKey = matcher.group(1);

            // 原始的元素属性值使用对应值（已在上面设置）

            // 调用PropertiesConfHandler类对应方法查询对应列表，保存到XmlConfWithProperties对应字段中
            List<com.adrninistrator.jacg.dto.writedb.WriteDbData4PropertiesConf> propConfList =
                    propertiesConfHandler.queryPropConfList(propKey, false);
            xmlConfWithProperties.setPropertiesConfList(propConfList);
        }

        return xmlConfWithProperties;
    }
}

