package com.adrninistrator.jacg.annotation.storage;

import com.adrninistrator.jacg.annotation.util.AnnotationAttributesParseUtil;
import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dboper.DbOperator;
import com.adrninistrator.jacg.dto.annotation_attribute.BaseAnnotationAttribute;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * @author adrninistrator
 * @date 2022/4/9
 * @description: 保存类及方法上的注解信息
 */
public class AnnotationStorage {
    private static final Logger logger = LoggerFactory.getLogger(AnnotationStorage.class);

    private final DbOperator dbOperator;

    private final DbOperWrapper dbOperWrapper;

    /*
        保存类注解信息，Map格式
        key 完整类名
        value
            注解属性
            key：
                注解类名
            value：
                注解属性Map
                key 属性名称
                value 属性值
     */
    private final Map<String, Map<String, Map<String, BaseAnnotationAttribute>>> classAnnotationInfoMap = new ConcurrentHashMap<>(100);

    /*
        保存方法注解信息，Map格式
        key 方法完整名称HASH+长度
        value
            注解属性
            key：
                注解类名
            value：
                注解属性Map
                key 属性名称
                value 属性值
     */
    private final Map<String, Map<String, Map<String, BaseAnnotationAttribute>>> methodAnnotationInfoMap = new ConcurrentHashMap<>(100);

    public AnnotationStorage(DbOperator dbOperator, DbOperWrapper dbOperWrapper) {
        this.dbOperator = dbOperator;
        this.dbOperWrapper = dbOperWrapper;
    }

    // 从数据库查询类或方法上的注解的属性
    private boolean queryClassOrMethodAnnotationAttributes(boolean handleClassOrMethod,
                                                           String classOrMethod,
                                                           List<Map<String, Object>> annotationList,
                                                           Map<String, Map<String, Map<String, BaseAnnotationAttribute>>> annotationWithAttributesMap) {
        if (annotationList == null) {
            return false;
        }

        logger.info("查询{}上注解的属性 {} 数量 {}", handleClassOrMethod ? "类" : "方法", classOrMethod, annotationList.size());

        if (annotationList.isEmpty()) {
            return true;
        }

        // 遍历查询到的注解
        for (Map<String, Object> annotationMap : annotationList) {
            String classNameOrMethodHash = (String) annotationMap.get(DC.ALIAS_ANNOTATION_CLASS_OR_METHOD);
            String annotationName = (String) annotationMap.get(DC.COMMON_ANNOTATION_ANNOTATION_NAME);

            Map<String, BaseAnnotationAttribute> tmpAttributeMap = new HashMap<>();
            // 当前方法HASH在Map中的value，若不存在则put，存在则get，避免覆盖
            Map<String, Map<String, BaseAnnotationAttribute>> tmpAnnotationMap = annotationWithAttributesMap.computeIfAbsent(classNameOrMethodHash, k -> new ConcurrentHashMap<>());
            tmpAnnotationMap.put(annotationName, tmpAttributeMap);

            // 查询当前注解的属性值
            String sql;
            if (handleClassOrMethod) {
                SqlKeyEnum sqlKeyEnum = SqlKeyEnum.CA_QUERY_ATTRIBUTE_BY_SIMPLE_CLASS_NAME;
                sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
                if (sql == null) {
                    sql = "select " + JACGSqlUtil.joinColumns(DC.COMMON_ANNOTATION_ATTRIBUTE_NAME, DC.COMMON_ANNOTATION_ATTRIBUTE_TYPE, DC.COMMON_ANNOTATION_ATTRIBUTE_VALUE) +
                            " from " + DbTableInfoEnum.DTIE_CLASS_ANNOTATION.getTableName(dbOperWrapper.getAppName()) +
                            " where " + DC.CA_SIMPLE_CLASS_NAME + " = ? and " + DC.COMMON_ANNOTATION_ANNOTATION_NAME + " = ?";
                    dbOperWrapper.cacheSql(sqlKeyEnum, sql);
                }
            } else {
                SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MA_QUERY_ATTRIBUTE_BY_METHOD_HASH;
                sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
                if (sql == null) {
                    sql = "select " + JACGSqlUtil.joinColumns(DC.COMMON_ANNOTATION_ATTRIBUTE_NAME, DC.COMMON_ANNOTATION_ATTRIBUTE_TYPE, DC.COMMON_ANNOTATION_ATTRIBUTE_VALUE) +
                            " from " + DbTableInfoEnum.DTIE_METHOD_ANNOTATION.getTableName(dbOperWrapper.getAppName()) +
                            " where " + DC.MA_METHOD_HASH + " = ? and " + DC.COMMON_ANNOTATION_ANNOTATION_NAME + " = ?";
                    dbOperWrapper.cacheSql(sqlKeyEnum, sql);
                }
            }

            List<Map<String, Object>> list = dbOperator.queryList(sql, new Object[]{classNameOrMethodHash, annotationName});
            if (list == null) {
                return false;
            }

            if (list.isEmpty()) {
                return true;
            }

            for (Map<String, Object> resultAttributesMap : list) {
                // 记录注解属性Map
                recordAttributeMap(resultAttributesMap, tmpAttributeMap);
            }
        }

        return true;
    }

    // 记录注解属性Map
    private void recordAttributeMap(Map<String, Object> resultAttributesMap, Map<String, BaseAnnotationAttribute> tmpAttributeMap) {
        String attributeName = (String) resultAttributesMap.get(DC.COMMON_ANNOTATION_ATTRIBUTE_NAME);
        if (StringUtils.isBlank(attributeName)) {
            // 对于未指定属性的注解，属性名称字段会是""，不需要put
            return;
        }

        String attributeType = (String) resultAttributesMap.get(DC.COMMON_ANNOTATION_ATTRIBUTE_TYPE);
        String attributeValue = (String) resultAttributesMap.get(DC.COMMON_ANNOTATION_ATTRIBUTE_VALUE);
        // 解析注解属性
        BaseAnnotationAttribute annotationAttribute = AnnotationAttributesParseUtil.parseFromDb(attributeType, attributeValue);

        tmpAttributeMap.put(attributeName, annotationAttribute);
    }

    // 从数据库查询类注解信息
    private boolean queryClassAnnotationInfo(String simpleClassName) {
        logger.info("从数据库查询类注解信息 {}", simpleClassName);
        // 查询有类的注解信息

        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.CA_QUERY_ANNOTATION_BY_SIMPLE_CLASS_NAME;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.joinColumns(DC.CA_SIMPLE_CLASS_NAME + " as " + DC.ALIAS_ANNOTATION_CLASS_OR_METHOD, DC.COMMON_ANNOTATION_ANNOTATION_NAME) +
                    " from " + DbTableInfoEnum.DTIE_CLASS_ANNOTATION.getTableName(dbOperWrapper.getAppName()) +
                    " where " + DC.CA_SIMPLE_CLASS_NAME + " = ?";
            dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        List<Map<String, Object>> annotationList = dbOperator.queryList(sql, new Object[]{simpleClassName});

        // 从数据库查询类上的注解的属性
        return queryClassOrMethodAnnotationAttributes(true, simpleClassName, annotationList, classAnnotationInfoMap);
    }

    // 从数据库查询方法注解信息
    private boolean queryMethodAnnotationInfo(String fullMethod, String methodHash) {
        logger.info("从数据库查询方法注解信息 {} {}", fullMethod, methodHash);
        // 查询有方法的注解信息
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MA_QUERY_ANNOTATION_BY_METHOD_HASH;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.joinColumns(DC.MA_METHOD_HASH + " as " + DC.ALIAS_ANNOTATION_CLASS_OR_METHOD, DC.COMMON_ANNOTATION_ANNOTATION_NAME) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_ANNOTATION.getTableName(dbOperWrapper.getAppName()) +
                    " where " + DC.MA_METHOD_HASH + " = ?";
            dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        List<Map<String, Object>> annotationList = dbOperator.queryList(sql, new Object[]{methodHash});

        // 从数据库查询方法上的注解的属性
        return queryClassOrMethodAnnotationAttributes(false, fullMethod, annotationList, methodAnnotationInfoMap);
    }

    /**
     * 根据完整类名获取对应的注解信息，Map格式
     *
     * @param className 完整类名或简单类名
     * @return
     */
    public Map<String, Map<String, BaseAnnotationAttribute>> getAnnotationMap4Class(String className) {
        String simpleClassName = dbOperWrapper.getSimpleClassName(className);
        Map<String, Map<String, BaseAnnotationAttribute>> existedInfo = classAnnotationInfoMap.get(simpleClassName);
        if (existedInfo != null) {
            return existedInfo;
        }

        // 从数据库查询类注解信息
        if (!queryClassAnnotationInfo(simpleClassName)) {
            classAnnotationInfoMap.put(simpleClassName, Collections.emptyMap());
            return Collections.emptyMap();
        }

        return classAnnotationInfoMap.get(simpleClassName);
    }

    /**
     * 根据完整方法HASH+长度获取对应的方法的注解信息，Map格式
     *
     * @param fullMethod 完整方法
     * @return
     */
    public Map<String, Map<String, BaseAnnotationAttribute>> getAnnotationMap4FullMethod(String fullMethod) {
        String methodHash = JACGUtil.genHashWithLen(fullMethod);
        Map<String, Map<String, BaseAnnotationAttribute>> existedInfo = methodAnnotationInfoMap.get(methodHash);
        if (existedInfo != null) {
            return existedInfo;
        }

        // 从数据库查询方法注解信息
        if (!queryMethodAnnotationInfo(fullMethod, methodHash)) {
            methodAnnotationInfoMap.put(methodHash, Collections.emptyMap());
            return Collections.emptyMap();
        }

        return methodAnnotationInfoMap.get(methodHash);
    }

    /**
     * 根据完整类名，及注解类名，获取对应的注解属性信息，Map格式
     *
     * @param className      完整类名
     * @param annotationName 注解类名
     * @return
     */
    public Map<String, BaseAnnotationAttribute> getAttributeMap4ClassAnnotation(String className, String annotationName) {
        Map<String, Map<String, BaseAnnotationAttribute>> map = getAnnotationMap4Class(className);
        if (map == null) {
            logger.warn("未找到指定类的注解信息 {}", className);
            return null;
        }

        return map.get(annotationName);
    }

    /**
     * 根据完整方法HASH+长度，及注解类名，获取对应的方法的注解属性信息，Map格式
     *
     * @param fullMethod     完整方法
     * @param annotationName 注解类名
     * @return
     */
    public Map<String, BaseAnnotationAttribute> getAttributeMap4MethodAnnotation(String fullMethod, String annotationName) {
        Map<String, Map<String, BaseAnnotationAttribute>> map = getAnnotationMap4FullMethod(fullMethod);
        if (map == null) {
            logger.warn("未找到指定方法HASH的注解信息 {}", fullMethod);
            return null;
        }

        return map.get(annotationName);
    }

    /**
     * 根据完整类名，及注解类名，及注解属性名，获取对应的注解属性值
     *
     * @param className      完整类名
     * @param annotationName 注解类名
     * @param attributeName  注解属性名
     * @return
     */
    public BaseAnnotationAttribute getAttributeInfo4ClassAnnotation(String className, String annotationName, String attributeName) {
        Map<String, BaseAnnotationAttribute> map = getAttributeMap4ClassAnnotation(className, annotationName);
        if (map == null) {
            logger.warn("类注解不存在 {} {}", className, annotationName);
            return null;
        }

        return map.get(attributeName);
    }

    /**
     * 根据完整方法HASH+长度，及注解类名，及注解属性名，获取对应的注解属性值
     *
     * @param fullMethod     完整方法
     * @param annotationName 注解类名
     * @param attributeName  注解属性名
     * @return
     */
    public BaseAnnotationAttribute getAttributeInfo4MethodAnnotation(String fullMethod, String annotationName, String attributeName) {
        Map<String, BaseAnnotationAttribute> map = getAttributeMap4MethodAnnotation(fullMethod, annotationName);
        if (map == null) {
            logger.warn("方法注解不存在 {} {}", fullMethod, annotationName);
            return null;
        }

        return map.get(attributeName);
    }

    /**
     * 根据完整类名，及注解类名，及注解属性名，获取预期类型的注解属性值
     *
     * @param className      完整类名
     * @param annotationName 注解类名
     * @param attributeName  注解属性名
     * @return attributeClassType 预期的注解属性类型
     * @return
     */
    @SuppressWarnings("unchecked")
    public <T extends BaseAnnotationAttribute> T getAttribute4ClassAnnotation(String className,
                                                                              String annotationName,
                                                                              String attributeName,
                                                                              Class<T> attributeClassType) {
        BaseAnnotationAttribute attribute = getAttributeInfo4ClassAnnotation(className, annotationName, attributeName);
        if (attribute == null) {
            logger.warn("类注解属性为空 {}\n{}\n{}", className, annotationName, attributeName);
            return null;
        }

        if (!attributeClassType.isAssignableFrom(attribute.getClass())) {
            logger.error("类注解属性的实现类型与预期不一致 {}\n{}\n{}\n{}\n{}", className, annotationName, attributeName,
                    attribute.getClass().getName(), attributeClassType.getName());
            return null;
        }

        return (T) attribute;
    }

    /**
     * 根据完整方法HASH+长度，及注解类名，及注解属性名，获取预期类型的注解属性值
     *
     * @param fullMethod     完整方法
     * @param annotationName 注解类名
     * @param attributeName  注解属性名
     * @param attributeName  注解属性名
     * @return attributeClassType 预期的注解属性类型
     */
    @SuppressWarnings("unchecked")
    public <T extends BaseAnnotationAttribute> T getAttribute4MethodAnnotation(String fullMethod,
                                                                               String annotationName,
                                                                               String attributeName,
                                                                               Class<T> attributeClassType) {
        BaseAnnotationAttribute attribute = getAttributeInfo4MethodAnnotation(fullMethod, annotationName, attributeName);
        if (attribute == null) {
            logger.warn("方法注解属性为空 {}\n{}\n{}", fullMethod, annotationName, attributeName);
            return null;
        }

        if (!attributeClassType.isAssignableFrom(attribute.getClass())) {
            logger.error("方法注解属性的实现类型与预期不一致 {}\n{}\n{}\n{}\n{}", fullMethod, annotationName, attributeName,
                    attribute.getClass().getName(), attributeClassType.getName());
            return null;
        }

        return (T) attribute;
    }

    /**
     * 从注解属性Map中，根据注解属性名，获取预期类型的注解属性值
     *
     * @param annotationAttributeMap 注解属性Map
     * @param attributeName          注解属性名
     * @return attributeClassType 预期的注解属性类型
     * @return
     */
    @SuppressWarnings("unchecked")
    public <T extends BaseAnnotationAttribute> T getAttributeFromMap(Map<String, BaseAnnotationAttribute> annotationAttributeMap,
                                                                     String attributeName,
                                                                     Class<T> attributeClassType) {
        if (annotationAttributeMap == null || attributeName == null || attributeClassType == null) {
            return null;
        }

        BaseAnnotationAttribute attribute = annotationAttributeMap.get(attributeName);
        if (attribute == null) {
            logger.warn("注解属性为空 {}", attributeName);
            return null;
        }

        if (!attributeClassType.isAssignableFrom(attribute.getClass())) {
            logger.error("类注解属性的实现类型与预期不一致 {}\n{}\n{}", attributeName, attribute.getClass().getName(), attributeClassType.getName());
            return null;
        }

        return (T) attribute;
    }
}
