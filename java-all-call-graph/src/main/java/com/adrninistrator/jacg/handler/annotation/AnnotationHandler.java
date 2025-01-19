package com.adrninistrator.jacg.handler.annotation;

import com.adrninistrator.jacg.annotation.util.AnnotationAttributesParseUtil;
import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.JACGCommonNameConstants;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.annotation.AnnotationAttributeInfo;
import com.adrninistrator.jacg.dto.annotation.AnnotationWithAttributeInfo;
import com.adrninistrator.jacg.dto.annotation.BaseAnnotationAttribute;
import com.adrninistrator.jacg.dto.annotation.StringAnnotationAttribute;
import com.adrninistrator.jacg.dto.annotation.SuperClassWithAnnotation;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4ClassAnnotation;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4FieldAnnotation;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodAnnotation;
import com.adrninistrator.jacg.extractor.common.enums.SpTxPropagationEnum;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.handler.extendsimpl.JACGExtendsImplHandler;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2023/1/6
 * @description: 注解相关的查询处理类
 */
public class AnnotationHandler extends BaseHandler {
    private static final Logger logger = LoggerFactory.getLogger(AnnotationHandler.class);

    public static final String ANNOTATION_ATTRIBUTE_COLUMNS = JACGSqlUtil.joinColumns(DC.COMMON_ANNOTATION_ATTRIBUTE_NAME, DC.COMMON_ANNOTATION_ATTRIBUTE_TYPE,
            DC.COMMON_ANNOTATION_ATTRIBUTE_VALUE);

    private final JACGExtendsImplHandler jacgExtendsImplHandler;

    public AnnotationHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
        jacgExtendsImplHandler = new JACGExtendsImplHandler(dbOperWrapper);
    }

    public AnnotationHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
        jacgExtendsImplHandler = new JACGExtendsImplHandler(dbOperWrapper);
    }

    /**
     * 查询带有指定注解的类名列表
     *
     * @param querySimpleClassName true: 查询唯一类名 false: 查询完整类名
     * @param annotationClassName  注解类名
     * @return
     */
    public List<String> queryClassesWithAnnotation(boolean querySimpleClassName, String annotationClassName) {
        SqlKeyEnum sqlKeyEnum = querySimpleClassName ? SqlKeyEnum.CA_QUERY_SIMPLE_CLASS_NAME_WITH_ANNOTATION : SqlKeyEnum.CA_QUERY_CLASS_NAME_WITH_ANNOTATION;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select distinct " + (querySimpleClassName ? DC.CA_SIMPLE_CLASS_NAME : DC.CA_CLASS_NAME) +
                    " from " + DbTableInfoEnum.DTIE_CLASS_ANNOTATION.getTableName() +
                    " where " + DC.CA_ANNOTATION_NAME + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryListOneColumn(sql, String.class, annotationClassName);
    }

    /**
     * 查询带有指定注解的完整方法列表
     *
     * @param queryFullMethod     true: 查询完整方法 false: 方法HASH+长度
     * @param annotationClassName 指定的注解类名
     * @return
     */
    public List<String> queryMethodsWithAnnotation(boolean queryFullMethod, String annotationClassName) {
        List<WriteDbData4MethodAnnotation> list = queryMethodsAndHashWithAnnotation(annotationClassName);
        if (list == null) {
            return null;
        }

        List<String> stringList = new ArrayList<>(list.size());
        for (WriteDbData4MethodAnnotation methodAnnotation : list) {
            stringList.add(queryFullMethod ? methodAnnotation.getFullMethod() : methodAnnotation.getMethodHash());
        }
        return stringList;
    }

    /**
     * 从方法注解表，查询带有指定注解的完整方法及方法HASH，结果会去重
     *
     * @param annotationClassName 注解类名
     * @return
     */
    public List<WriteDbData4MethodAnnotation> queryMethodsAndHashWithAnnotation(String annotationClassName) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MA_QUERY_FMAH_WITH_ANNOTATIONS;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select distinct " + JACGSqlUtil.joinColumns(DC.MA_FULL_METHOD, DC.MA_METHOD_HASH) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_ANNOTATION.getTableName() +
                    " where " + DC.MA_ANNOTATION_NAME + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryList(sql, WriteDbData4MethodAnnotation.class, annotationClassName);
    }

    /**
     * 根据完整方法，查询方法指定注解的指定属性
     *
     * @param fullMethod     完整方法
     * @param annotationName 注解类名
     * @param attributeName  注解属性名
     * @param attributeName  注解属性名
     * @return attributeClassType 预期的注解属性类型
     */
    @SuppressWarnings("unchecked")
    public <T extends BaseAnnotationAttribute> T queryAttribute4MethodAnnotation(String fullMethod,
                                                                                 String annotationName,
                                                                                 String attributeName,
                                                                                 Class<T> attributeClassType) {
        String methodHash = JACGUtil.genHashWithLen(fullMethod);
        logger.debug("查询方法指定注解的指定属性 {} {} {} {}", fullMethod, methodHash, annotationName, attributeName);

        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MA_QUERY_SINGLE_ATTRIBUTE_BY_METHOD_HASH;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.joinColumns(DC.MA_ATTRIBUTE_TYPE, DC.MA_ATTRIBUTE_VALUE) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_ANNOTATION.getTableName() +
                    " where " + DC.MA_METHOD_HASH + " = ?" +
                    " and " + DC.MA_ANNOTATION_NAME + " = ?" +
                    " and " + DC.MA_ATTRIBUTE_NAME + " = ?" +
                    " limit 1";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        AnnotationAttributeInfo annotationAttributeInfo = dbOperator.queryObject(sql, AnnotationAttributeInfo.class, methodHash, annotationName, attributeName);
        if (annotationAttributeInfo == null) {
            return null;
        }

        // 根据查询的结果获取对应的注解属性值
        BaseAnnotationAttribute attribute = AnnotationAttributesParseUtil.genAnnotationAttribute(annotationAttributeInfo);
        if (!attributeClassType.isAssignableFrom(attribute.getClass())) {
            logger.error("方法注解属性的实现类型与预期不一致 {} {} {} {} {}", fullMethod, annotationName, attributeName, attribute.getClass().getName(), attributeClassType.getName());
            return null;
        }
        return (T) attribute;
    }

    /**
     * 根据完整方法，及注解类名获取对应的方法的注解信息
     *
     * @param fullMethod
     * @param annotationName
     * @return key：注解属性名称，value：注解属性
     */
    public Map<String, BaseAnnotationAttribute> queryMethodAnnotationAttributes(String fullMethod, String annotationName) {
        String methodHash = JACGUtil.genHashWithLen(fullMethod);
        return queryMethodAnnotationAttributes(fullMethod, methodHash, annotationName);
    }

    /**
     * 根据完整方法，及注解类名获取对应的方法的注解信息
     *
     * @param fullMethod     仅用于打印日志
     * @param methodHash
     * @param annotationName
     * @return key：注解属性名称，value：注解属性
     */
    public Map<String, BaseAnnotationAttribute> queryMethodAnnotationAttributes(String fullMethod, String methodHash, String annotationName) {
        logger.debug("查询方法上注解的属性 {} {}", fullMethod, annotationName);
        String sql;
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MA_QUERY_ALL_ATTRIBUTES;
        sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + ANNOTATION_ATTRIBUTE_COLUMNS +
                    " from " + DbTableInfoEnum.DTIE_METHOD_ANNOTATION.getTableName() +
                    " where " + DC.MA_METHOD_HASH + " = ?" +
                    " and " + DC.MA_ANNOTATION_NAME + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        List<AnnotationAttributeInfo> list = dbOperator.queryList(sql, AnnotationAttributeInfo.class, methodHash, annotationName);
        // 将查询到的注解属性列表转换为对应的Map
        return genAnnotationAttributeMap(list);
    }

    /**
     * 查询Spring事务注解@Transactional对应的事务传播行为，仅当确认对应方法上有@Transactional注解时，才能使用当前方法查询
     *
     * @param fullMethod
     * @return
     */
    public String querySpringTxAnnotationPropagation(String fullMethod) {
        StringAnnotationAttribute propagationAttribute = queryAttribute4MethodAnnotation(fullMethod,
                JACGCommonNameConstants.SPRING_TX_ANNOTATION,
                JACGCommonNameConstants.SPRING_TX_ATTRIBUTE_PROPAGATION,
                StringAnnotationAttribute.class);
        if (propagationAttribute == null) {
            return SpTxPropagationEnum.STPE_DEFAULT_REQUIRED.getPropagation();
        }
        return propagationAttribute.getAttributeString();
    }

    /**
     * 根据完整类名获取对应的注解信息，Map格式
     *
     * @param className 完整类名
     * @return
     */
    public Map<String, Map<String, BaseAnnotationAttribute>> queryAnnotationMap4Class(String className) {
        /*
            返回的Map格式
                key     注解类名
                value   Map<String, BaseAnnotationAttribute> key：注解属性名称，value：注解属性
         */
        String simpleClassName = dbOperWrapper.querySimpleClassName(className);
        logger.debug("根据完整类名获取对应的注解信息 {}", simpleClassName);

        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.CA_QUERY_ANNOTATIONS_BY_SIMPLE_CLASS_NAME;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.joinColumns(DC.CA_ANNOTATION_NAME, DC.CA_ATTRIBUTE_NAME, DC.CA_ATTRIBUTE_TYPE, DC.CA_ATTRIBUTE_VALUE) +
                    " from " + DbTableInfoEnum.DTIE_CLASS_ANNOTATION.getTableName() +
                    " where " + DC.CA_SIMPLE_CLASS_NAME + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        List<AnnotationWithAttributeInfo> annotationList = dbOperator.queryList(sql, AnnotationWithAttributeInfo.class, simpleClassName);
        // 根据从数据库的查询结果生成注解对应的Map信息
        return genAnnotationMapFromQueryResult(annotationList);
    }

    /**
     * 获取指定类上指定注解对应的注解属性
     *
     * @param className      完整类名
     * @param annotationName 注解类名
     * @return 若返回map isEmpty()为true，代表类上没有对应的注解。若返回map isEmpty()为false，代表代表类上有对应的注解，key：注解属性名称，value：注解属性
     */
    public Map<String, BaseAnnotationAttribute> queryAnnotationAttributes4Class(String className, String annotationName) {
        String simpleClassName = dbOperWrapper.querySimpleClassName(className);
        logger.debug("获取指定类上指定注解对应的注解属性 {}", simpleClassName);

        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.CA_QUERY_ONE_ANNOTATION_BY_SIMPLE_CLASS_NAME;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + ANNOTATION_ATTRIBUTE_COLUMNS +
                    " from " + DbTableInfoEnum.DTIE_CLASS_ANNOTATION.getTableName() +
                    " where " + DC.CA_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.CA_ANNOTATION_NAME + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        List<AnnotationAttributeInfo> list = dbOperator.queryList(sql, AnnotationAttributeInfo.class, simpleClassName, annotationName);
        // 将查询到的注解属性列表转换为对应的Map
        return genAnnotationAttributeMap(list);
    }

    // 根据从数据库的查询结果生成注解对应的Map信息
    private Map<String, Map<String, BaseAnnotationAttribute>> genAnnotationMapFromQueryResult(List<AnnotationWithAttributeInfo> list) {
        if (JavaCG2Util.isCollectionEmpty(list)) {
            return Collections.emptyMap();
        }
        Map<String, Map<String, BaseAnnotationAttribute>> resultMap = new HashMap<>();
        for (AnnotationWithAttributeInfo annotationWithAttributeInfo : list) {
            // 根据查询的结果获取对应的注解属性值
            BaseAnnotationAttribute annotationAttribute = AnnotationAttributesParseUtil.genAnnotationAttribute(annotationWithAttributeInfo);
            Map<String, BaseAnnotationAttribute> attributeMap = resultMap.computeIfAbsent(annotationWithAttributeInfo.getAnnotationName(), k -> new HashMap<>());
            attributeMap.put(annotationWithAttributeInfo.getAttributeName(), annotationAttribute);
        }
        return resultMap;
    }

    /**
     * 根据完整方法HASH+长度获取对应的方法的注解信息，Map格式
     *
     * @param fullMethod 完整方法
     * @return
     */
    public Map<String, Map<String, BaseAnnotationAttribute>> queryAnnotationMap4FullMethod(String fullMethod) {
        /*
            返回的Map格式
                key     注解类名
                value   Map<String, BaseAnnotationAttribute> key：注解属性名称，value：注解属性
         */
        String methodHash = JACGUtil.genHashWithLen(fullMethod);
        logger.debug("从数据库查询方法注解信息 {} {}", fullMethod, methodHash);
        // 查询有方法的注解信息
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MA_QUERY_ANNOTATION_BY_METHOD_HASH;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.joinColumns(DC.MA_METHOD_HASH, DC.MA_ANNOTATION_NAME, DC.MA_ATTRIBUTE_NAME, DC.MA_ATTRIBUTE_TYPE, DC.MA_ATTRIBUTE_VALUE) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_ANNOTATION.getTableName() +
                    " where " + DC.MA_METHOD_HASH + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        List<AnnotationWithAttributeInfo> annotationList = dbOperator.queryList(sql, AnnotationWithAttributeInfo.class, methodHash);
        // 根据从数据库的查询结果生成注解对应的Map信息
        return genAnnotationMapFromQueryResult(annotationList);
    }

    /**
     * 查询指定类的所有父类上指定的注解属性
     *
     * @param className      指定类名
     * @param annotationName 指定的注解类名
     * @return
     */
    public List<SuperClassWithAnnotation> querySuperClassesInfo(String className, String annotationName) {
        List<SuperClassWithAnnotation> superClassWithAnnotationList = new ArrayList<>();
        String currentClassName = className;
        while (true) {
            // 查询当前类的父类名称
            String superClassName = jacgExtendsImplHandler.querySuperClassNameByFull(currentClassName);
            if (superClassName == null) {
                break;
            }
            // 获取指定类上指定注解对应的注解属性
            Map<String, BaseAnnotationAttribute> annotationAttributeMap = queryAnnotationAttributes4Class(superClassName, annotationName);
            SuperClassWithAnnotation superClassWithAnnotation = new SuperClassWithAnnotation(superClassName, annotationAttributeMap);
            superClassWithAnnotationList.add(superClassWithAnnotation);
            // 继续查询当前父类的父类
            currentClassName = superClassName;
        }
        return superClassWithAnnotationList;
    }

    /**
     * 查询在字段上有指定注解的字段及对应的类信息
     *
     * @param annotationClassName
     * @return
     */
    public List<WriteDbData4FieldAnnotation> queryClassFieldsWithAnnotation(String annotationClassName) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.FA_QUERY_CLASS_FIELD_WITH_ANNOTATIONS;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select distinct " + JACGSqlUtil.joinColumns(DC.FA_SIMPLE_CLASS_NAME, DC.FA_FIELD_NAME, DC.FA_CLASS_NAME) +
                    " from " + DbTableInfoEnum.DTIE_FIELD_ANNOTATION.getTableName() +
                    " where " + DC.FA_ANNOTATION_NAME + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryList(sql, WriteDbData4FieldAnnotation.class, annotationClassName);
    }

    /**
     * 获取指定字段上指定注解对应的注解属性
     *
     * @param className      完整类名
     * @param fieldName      字段名称
     * @param annotationName 注解类名
     * @return 若返回map isEmpty()为true，代表类上没有对应的注解。若返回map isEmpty()为false，代表代表类上有对应的注解，key：注解属性名称，value：注解属性
     */
    public Map<String, BaseAnnotationAttribute> queryAnnotationAttributes4Field(String className, String fieldName, String annotationName) {
        String simpleClassName = dbOperWrapper.querySimpleClassName(className);
        logger.debug("获取指定字段上指定注解对应的注解属性 {} {}", simpleClassName, fieldName);
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.FA_QUERY_ONE_ANNOTATION_BY_CLASS_FIELD;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + ANNOTATION_ATTRIBUTE_COLUMNS +
                    " from " + DbTableInfoEnum.DTIE_FIELD_ANNOTATION.getTableName() +
                    " where " + DC.FA_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.FA_FIELD_NAME + " = ?" +
                    " and " + DC.FA_ANNOTATION_NAME + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        List<AnnotationAttributeInfo> list = dbOperator.queryList(sql, AnnotationAttributeInfo.class, simpleClassName, fieldName, annotationName);
        // 将查询到的注解属性列表转换为对应的Map
        return genAnnotationAttributeMap(list);
    }

    /**
     * 根据指定的注解及属性，查询对应的类的注解信息
     *
     * @param annotationName 注解名称
     * @param attributeName  注解属性名称
     * @param attributeValue 注解属性值
     * @return
     */
    public List<WriteDbData4ClassAnnotation> queryClassAnnotationByAnnotationAttribute(String annotationName, String attributeName, String attributeValue) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.CA_QUERY_ONE_ANNOTATION_BY_ANNOTATION_ATTRIBUTE;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_CLASS_ANNOTATION) +
                    " from " + DbTableInfoEnum.DTIE_CLASS_ANNOTATION.getTableName() +
                    " where " + DC.CA_ANNOTATION_NAME + " = ?" +
                    " and " + DC.CA_ATTRIBUTE_NAME + " = ?" +
                    " and " + DC.CA_ATTRIBUTE_VALUE + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryList(sql, WriteDbData4ClassAnnotation.class, annotationName, attributeName, attributeValue);
    }

    /**
     * 将查询到的注解属性列表转换为对应的Map
     *
     * @param list
     * @return
     */
    private Map<String, BaseAnnotationAttribute> genAnnotationAttributeMap(List<AnnotationAttributeInfo> list) {
        if (JavaCG2Util.isCollectionEmpty(list)) {
            return Collections.emptyMap();
        }

        Map<String, BaseAnnotationAttribute> annotationAttributeMap = new HashMap<>(list.size());
        for (AnnotationAttributeInfo annotationAttributeInfo : list) {
            // 根据查询的结果获取对应的注解属性值
            annotationAttributeMap.put(annotationAttributeInfo.getAttributeName(), AnnotationAttributesParseUtil.genAnnotationAttribute(annotationAttributeInfo));
        }
        return annotationAttributeMap;
    }

    /**
     * 获取字段上的@JsonProperty注解的value属性值
     *
     * @param className 类名
     * @param fieldName 字段名
     * @return
     */
    public String queryFieldJsonPropertyValue(String className, String fieldName) {
        Map<String, BaseAnnotationAttribute> fieldAnnotationAttributeMap = queryAnnotationAttributes4Field(className, fieldName,
                JACGCommonNameConstants.JSON_PROPERTY_ANNOTATION_NAME_);
        String jsonPropertyValue = AnnotationAttributesParseUtil.getAttributeStringValue(fieldAnnotationAttributeMap, JACGCommonNameConstants.ANNOTATION_ATTRIBUTE_NAME_VALUE);
        // @JsonProperty注解属性值若为""，也当作null
        return StringUtils.isBlank(jsonPropertyValue) ? null : jsonPropertyValue;
    }

    /**
     * 根据完整方法，查询方法指定参数的注解的指定属性
     *
     * @param fullMethod     完整方法
     * @param argSeq         参数序号，从0开始
     * @param annotationName 注解类名
     * @param attributeName  注解属性名
     * @param attributeName  注解属性名
     * @return attributeClassType 预期的注解属性类型
     */
    @SuppressWarnings("unchecked")
    public <T extends BaseAnnotationAttribute> T queryAttribute4MethodArgAnnotation(String fullMethod,
                                                                                    int argSeq,
                                                                                    String annotationName,
                                                                                    String attributeName,
                                                                                    Class<T> attributeClassType) {
        String methodHash = JACGUtil.genHashWithLen(fullMethod);
        logger.debug("查询方法指定参数的指定注解的指定属性 {} {} {} {} {}", fullMethod, argSeq, methodHash, annotationName, attributeName);

        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MAA_QUERY_SINGLE_ATTRIBUTE_BY_METHOD_HASH;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.joinColumns(DC.MA_ATTRIBUTE_TYPE, DC.MA_ATTRIBUTE_VALUE) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_ARG_ANNOTATION.getTableName() +
                    " where " + DC.MAA_METHOD_HASH + " = ?" +
                    " and " + DC.MAA_ARG_SEQ + " = ?" +
                    " and " + DC.MAA_ANNOTATION_NAME + " = ?" +
                    " and " + DC.MAA_ATTRIBUTE_NAME + " = ?" +
                    " limit 1";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        AnnotationAttributeInfo annotationAttributeInfo = dbOperator.queryObject(sql, AnnotationAttributeInfo.class, methodHash, argSeq, annotationName, attributeName);
        if (annotationAttributeInfo == null) {
            return null;
        }

        // 根据查询的结果获取对应的注解属性值
        BaseAnnotationAttribute attribute = AnnotationAttributesParseUtil.genAnnotationAttribute(annotationAttributeInfo);
        if (!attributeClassType.isAssignableFrom(attribute.getClass())) {
            logger.error("方法注解属性的实现类型与预期不一致 {} {} {} {} {} {}", fullMethod, argSeq, annotationName, attributeName, attribute.getClass().getName(), attributeClassType.getName());
            return null;
        }
        return (T) attribute;
    }
}