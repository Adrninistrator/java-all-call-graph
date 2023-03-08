package com.adrninistrator.jacg.handler.annotation;

import com.adrninistrator.jacg.annotation.util.AnnotationAttributesParseUtil;
import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.JACGCommonNameConstants;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dboper.DbOperator;
import com.adrninistrator.jacg.dto.annotation_attribute.BaseAnnotationAttribute;
import com.adrninistrator.jacg.dto.annotation_attribute.StringAnnotationAttribute;
import com.adrninistrator.jacg.dto.method.MethodAndHash;
import com.adrninistrator.jacg.extractor.common.enums.SpTxPropagationEnum;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2023/1/6
 * @description: 注解相关的查询处理类
 */
public class AnnotationHandler extends BaseHandler {
    private static final Logger logger = LoggerFactory.getLogger(AnnotationHandler.class);

    public AnnotationHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
    }

    public AnnotationHandler(DbOperator dbOperator, DbOperWrapper dbOperWrapper) {
        super(dbOperator, dbOperWrapper);
    }

    /**
     * 查询带有指定注解的类名列表
     *
     * @param querySimpleClassName true: 查询唯一类名 false: 查询完整类名
     * @param annotationClassNames
     * @return
     */
    public List<String> queryClassesWithAnnotations(boolean querySimpleClassName, String... annotationClassNames) {
        return dbOperWrapper.getClassesWithAnnotations(querySimpleClassName, annotationClassNames);
    }

    /**
     * 查询带有指定注解的方法列表
     *
     * @param annotationClassNames 指定的注解类名
     * @return
     */
    public List<MethodAndHash> queryMethodsWithAnnotations(String... annotationClassNames) {
        return dbOperWrapper.getMethodsAndHashWithAnnotations(annotationClassNames);
    }

    /**
     * 查询带有指定注解的完整方法列表
     *
     * @param queryFullMethod      true: 查询完整方法 false: 方法HASH+长度
     * @param annotationClassNames 指定的注解类名
     * @return
     */
    public List<String> queryMethodsWithAnnotations(boolean queryFullMethod, String... annotationClassNames) {
        List<MethodAndHash> list = dbOperWrapper.getMethodsAndHashWithAnnotations(annotationClassNames);
        if (list == null) {
            return null;
        }

        List<String> stringList = new ArrayList<>(list.size());
        for (MethodAndHash methodAndHash : list) {
            stringList.add(queryFullMethod ? methodAndHash.getFullMethod() : methodAndHash.getMethodHash());
        }
        return stringList;
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
            sql = "select " + JACGSqlUtil.joinColumns(DC.COMMON_ANNOTATION_ATTRIBUTE_TYPE, DC.COMMON_ANNOTATION_ATTRIBUTE_VALUE) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_ANNOTATION.getTableName(dbOperWrapper.getAppName()) +
                    " where " + DC.MA_METHOD_HASH + " = ?" +
                    " and " + DC.COMMON_ANNOTATION_ANNOTATION_NAME + " = ?" +
                    " and " + DC.COMMON_ANNOTATION_ATTRIBUTE_NAME + " = ?";
            dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        Map<String, Object> map = dbOperator.queryOneRow(sql, new Object[]{methodHash, annotationName, attributeName});
        if (JACGUtil.isMapEmpty(map)) {
            return null;
        }

        String attributeType = (String) map.get(DC.COMMON_ANNOTATION_ATTRIBUTE_TYPE);
        String attributeValue = (String) map.get(DC.COMMON_ANNOTATION_ATTRIBUTE_VALUE);
        // 解析注解属性
        BaseAnnotationAttribute attribute = AnnotationAttributesParseUtil.parseFromDb(attributeType, attributeValue);
        if (!attributeClassType.isAssignableFrom(attribute.getClass())) {
            logger.error("方法注解属性的实现类型与预期不一致 {}\n{}\n{}\n{}\n{}", fullMethod, annotationName, attributeName,
                    attribute.getClass().getName(), attributeClassType.getName());
            return null;
        }

        return (T) attribute;
    }

    /**
     * 查询Spring事务注解@Transactional对应的事务传播行为
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
}