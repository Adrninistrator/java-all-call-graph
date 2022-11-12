package com.adrninistrator.jacg.annotation;

import com.adrninistrator.jacg.common.CommonAnnotationConstants;
import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.dboper.DbOperator;
import com.adrninistrator.jacg.dto.annotation.MethodWithAnnotationInfo;
import com.adrninistrator.jacg.dto.annotation_attribute.BaseAnnotationAttribute;
import com.adrninistrator.jacg.dto.annotation_attribute.StringAnnotationAttribute;
import com.adrninistrator.jacg.extensions.annotation_attributes.AllAnnotationAttributesPraser;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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

    private DbOperator dbOperator;

    private String appName;

    // 完成初始化标志
    private boolean inited;

    /*
        保存方法注解信息，List格式
        key 方法完整名称HASH+长度
        value 注解信息
     */
    private final Map<String, MethodWithAnnotationInfo> methodWithAnnotationInfoHashMap = new ConcurrentHashMap<>(100);

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

    public AnnotationStorage(DbOperator dbOperator, String appName) {
        this.dbOperator = dbOperator;
        this.appName = appName;
    }

    private boolean checkInited() {
        if (!inited) {
            logger.error("未完成初始化，请先调用初始化方法");
        }
        return inited;
    }

    /**
     * 初始化
     *
     * @return
     */
    public boolean init() {
        if (inited) {
            logger.warn("已完成初始化，若需要再次初始化，可修改初始标志后再执行当前方法");
            return true;
        }

        // 从数据库查询类注解信息
        if (!queryClassAnnotationInfo()) {
            return false;
        }

        // 从数据库查询方法注解信息
        if (!queryMethodAnnotationInfo()) {
            return false;
        }

        inited = true;
        return true;
    }

    // 从数据库查询类或方法上的注解的属性
    private boolean queryClassOrMethodAnnotationAttributes(boolean handleClassOrMethod,
                                                           List<Map<String, Object>> annotationList,
                                                           Map<String, Map<String, Map<String, BaseAnnotationAttribute>>> annotationWithAttributesMap) {
        if (annotationList == null) {
            return false;
        }

        logger.info("查询{}上注解的属性，数量 {}", handleClassOrMethod ? "类" : "方法", annotationList.size());

        if (annotationList.isEmpty()) {
            return true;
        }

        // 遍历查询到的注解
        for (Map<String, Object> annotationMap : annotationList) {
            String classNameOrMethodHash = (String) annotationMap.get(DC.ALIAS_ANNOTATION_CLASS_OR_METHOD);
            String annotationName = (String) annotationMap.get(DC.COMMON_ANNOTATION_ANNOTATION_NAME);

            Map<String, BaseAnnotationAttribute> tmpAttributeMap = new HashMap<>();
            // 当前方法HASH在Map中的value，若不存在则put，存在则get，避免覆盖
            Map<String, Map<String, BaseAnnotationAttribute>> tmpAnnotationMap = annotationWithAttributesMap.computeIfAbsent(classNameOrMethodHash, k -> new HashMap<>());
            tmpAnnotationMap.put(annotationName, tmpAttributeMap);

            // 查询当前注解的属性值
            String sql;
            if (handleClassOrMethod) {
                sql = "select " + JACGSqlUtil.joinColumns(DC.COMMON_ANNOTATION_ATTRIBUTE_NAME, DC.COMMON_ANNOTATION_ATTRIBUTE_VALUE) +
                        " from " + JACGConstants.TABLE_PREFIX_CLASS_ANNOTATION + appName +
                        " where " + DC.CA_FULL_CLASS_NAME + " = ? and " + DC.COMMON_ANNOTATION_ANNOTATION_NAME + " = ?";
            } else {
                sql = "select " + JACGSqlUtil.joinColumns(DC.COMMON_ANNOTATION_ATTRIBUTE_NAME, DC.COMMON_ANNOTATION_ATTRIBUTE_VALUE) +
                        " from " + JACGConstants.TABLE_PREFIX_METHOD_ANNOTATION + appName +
                        " where " + DC.MA_METHOD_HASH + " = ? and " + DC.COMMON_ANNOTATION_ANNOTATION_NAME + " = ?";
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

        String attributeValue = (String) resultAttributesMap.get(DC.COMMON_ANNOTATION_ATTRIBUTE_VALUE);
        // 解析注解属性
        BaseAnnotationAttribute annotationAttribute = AllAnnotationAttributesPraser.parse(attributeValue);

        tmpAttributeMap.put(attributeName, annotationAttribute);
    }

    // 从数据库查询类注解信息
    private boolean queryClassAnnotationInfo() {
        logger.info("从数据库查询类注解信息");
        // 查询有注解的类信息，包含类名，注解类名
        String sql = "select " + JACGSqlUtil.joinColumns(DC.CA_FULL_CLASS_NAME + " as " + DC.ALIAS_ANNOTATION_CLASS_OR_METHOD,
                DC.COMMON_ANNOTATION_ANNOTATION_NAME) +
                " from " + JACGConstants.TABLE_PREFIX_CLASS_ANNOTATION + appName +
                " group by " + JACGSqlUtil.joinColumns(DC.CA_FULL_CLASS_NAME, DC.COMMON_ANNOTATION_ANNOTATION_NAME);

        List<Map<String, Object>> list = dbOperator.queryList(sql, null);

        // 从数据库查询类上的注解的属性
        return queryClassOrMethodAnnotationAttributes(true, list, classAnnotationInfoMap);
    }

    // 从数据库查询方法注解信息
    private boolean queryMethodAnnotationInfo() {
        logger.info("从数据库查询方法注解信息");
        // 查询有注解的方法信息，包含方法HASH，注解类名
        String sql = "select " + JACGSqlUtil.joinColumns(DC.MA_METHOD_HASH + " as " + DC.ALIAS_ANNOTATION_CLASS_OR_METHOD,
                DC.COMMON_ANNOTATION_ANNOTATION_NAME) +
                " from " + JACGConstants.TABLE_PREFIX_METHOD_ANNOTATION + appName +
                " group by " + JACGSqlUtil.joinColumns(DC.MA_METHOD_HASH, DC.COMMON_ANNOTATION_ANNOTATION_NAME);

        List<Map<String, Object>> list = dbOperator.queryList(sql, null);

        // 从数据库查询方法上的注解的属性
        if (!queryClassOrMethodAnnotationAttributes(false, list, methodAnnotationInfoMap)) {
            return false;
        }

        // 查询方法HASH对应的完整方法
        return queryMethodHashAndFullMethod();
    }

    // 查询方法HASH对应的完整方法
    private boolean queryMethodHashAndFullMethod() {
        String sql = "select " + JACGSqlUtil.joinColumns(DC.MA_METHOD_HASH, DC.MA_FULL_METHOD) +
                " from " + JACGConstants.TABLE_PREFIX_METHOD_ANNOTATION + appName +
                " group by " + JACGSqlUtil.joinColumns(DC.MA_METHOD_HASH, DC.MA_FULL_METHOD);

        List<Map<String, Object>> list = dbOperator.queryList(sql, null);
        if (list == null) {
            return false;
        }

        for (Map<String, Object> map : list) {
            String methodHash = (String) map.get(DC.MA_METHOD_HASH);
            String fullMethod = (String) map.get(DC.MA_FULL_METHOD);

            methodWithAnnotationInfoHashMap.put(methodHash, new MethodWithAnnotationInfo(fullMethod, JACGUtil.getFullClassNameFromMethod(fullMethod)));
        }

        return true;
    }

    /**
     * 根据完整方法HASH+长度获取对应的方法及注解信息，List格式
     *
     * @param methodHash 完整方法HASH+长度
     * @return
     */
    public MethodWithAnnotationInfo getMethodWithAnnotationInfo(String methodHash) {
        if (!checkInited()) {
            return null;
        }

        return methodWithAnnotationInfoHashMap.get(methodHash);
    }

    /**
     * 根据完整类名获取对应的注解信息，Map格式
     *
     * @param fullClassName 完整类名
     * @return
     */
    public Map<String, Map<String, BaseAnnotationAttribute>> getAnnotationMap4Class(String fullClassName) {
        if (!checkInited()) {
            return null;
        }

        return classAnnotationInfoMap.get(fullClassName);
    }

    /**
     * 根据完整方法HASH+长度获取对应的方法的注解信息，Map格式
     *
     * @param methodHash 完整方法HASH+长度
     * @return
     */
    public Map<String, Map<String, BaseAnnotationAttribute>> getAnnotationMap4Method(String methodHash) {
        if (!checkInited()) {
            return null;
        }

        return methodAnnotationInfoMap.get(methodHash);
    }

    /**
     * 根据完整类名，及注解类名，获取对应的注解属性信息，Map格式
     *
     * @param fullClassName  完整类名
     * @param annotationName 注解类名
     * @return
     */
    public Map<String, BaseAnnotationAttribute> getAttributeMap4ClassAnnotation(String fullClassName, String annotationName) {
        Map<String, Map<String, BaseAnnotationAttribute>> map = getAnnotationMap4Class(fullClassName);
        if (map == null) {
            logger.warn("未找到指定类的注解信息 {}", fullClassName);
            return null;
        }

        return map.get(annotationName);
    }

    /**
     * 根据完整方法HASH+长度，及注解类名，获取对应的方法的注解属性信息，Map格式
     *
     * @param methodHash     完整方法HASH+长度
     * @param annotationName 注解类名
     * @return
     */
    public Map<String, BaseAnnotationAttribute> getAttributeMap4MethodAnnotation(String methodHash, String annotationName) {
        Map<String, Map<String, BaseAnnotationAttribute>> map = getAnnotationMap4Method(methodHash);
        if (map == null) {
            logger.warn("未找到指定方法HASH的注解信息 {}", methodHash);
            return null;
        }

        return map.get(annotationName);
    }

    /**
     * 根据完整类名，及注解类名，及注解属性名，获取对应的注解属性值
     *
     * @param fullClassName  完整类名
     * @param annotationName 注解类名
     * @param attributeName  注解属性名
     * @return
     */
    public BaseAnnotationAttribute getAttributeInfo4ClassAnnotation(String fullClassName, String annotationName, String attributeName) {
        Map<String, BaseAnnotationAttribute> map = getAttributeMap4ClassAnnotation(fullClassName, annotationName);
        if (map == null) {
            logger.warn("类注解不存在 {}\n{}", fullClassName, annotationName);
            return null;
        }

        return map.get(attributeName);
    }

    /**
     * 根据完整方法HASH+长度，及注解类名，及注解属性名，获取对应的注解属性值
     *
     * @param methodHash     完整方法HASH+长度
     * @param annotationName 注解类名
     * @param attributeName  注解属性名
     * @return
     */
    public BaseAnnotationAttribute getAttributeInfo4MethodAnnotation(String methodHash, String annotationName, String attributeName) {
        Map<String, BaseAnnotationAttribute> map = getAttributeMap4MethodAnnotation(methodHash, annotationName);
        if (map == null) {
            logger.warn("方法注解不存在 {}\n{}", methodHash, annotationName);
            return null;
        }

        return map.get(attributeName);
    }

    /**
     * 根据完整类名，及注解类名，及注解属性名，获取预期类型的注解属性值
     *
     * @param fullClassName  完整类名
     * @param annotationName 注解类名
     * @param attributeName  注解属性名
     * @return attributeClassType 预期的注解属性类型
     * @return
     */
    public <T extends BaseAnnotationAttribute> T getAttribute4ClassAnnotation(String fullClassName,
                                                                              String annotationName,
                                                                              String attributeName,
                                                                              Class<T> attributeClassType) {
        BaseAnnotationAttribute attribute = getAttributeInfo4ClassAnnotation(fullClassName, annotationName, attributeName);
        if (attribute == null) {
            logger.warn("类注解属性为空 {}\n{}\n{}", fullClassName, annotationName, attributeName);
            return null;
        }

        if (!attributeClassType.isAssignableFrom(attribute.getClass())) {
            logger.error("类注解属性的实现类型与预期不一致 {}\n{}\n{}\n{}\n{}", fullClassName, annotationName, attributeName,
                    attribute.getClass().getName(), attributeClassType.getName());
            return null;
        }

        return (T) attribute;
    }

    /**
     * 根据完整方法HASH+长度，及注解类名，及注解属性名，获取预期类型的注解属性值
     *
     * @param methodHash     完整方法HASH+长度
     * @param annotationName 注解类名
     * @param attributeName  注解属性名
     * @param attributeName  注解属性名
     * @return attributeClassType 预期的注解属性类型
     */
    public <T extends BaseAnnotationAttribute> T getAttribute4MethodAnnotation(String methodHash,
                                                                               String annotationName,
                                                                               String attributeName,
                                                                               Class<T> attributeClassType) {
        BaseAnnotationAttribute attribute = getAttributeInfo4MethodAnnotation(methodHash, annotationName, attributeName);
        if (attribute == null) {
            logger.warn("方法注解属性为空 {}\n{}\n{}", methodHash, annotationName, attributeName);
            return null;
        }

        if (!attributeClassType.isAssignableFrom(attribute.getClass())) {
            logger.error("方法注解属性的实现类型与预期不一致 {}\n{}\n{}\n{}\n{}", methodHash, annotationName, attributeName,
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

    /**
     * 获取Spring的Bean信息
     *
     * @param beanNameAndClassMap 可为空，key: bean的名称 value: bean的完整类名
     * @param classAndBeanNameMap 可为空，key: bean的完整类名 value: bean的名称
     * @return
     */
    public boolean getSpringBeanMap(Map<String, String> beanNameAndClassMap, Map<String, String> classAndBeanNameMap) {
        if (!checkInited()) {
            return false;
        }

        if (beanNameAndClassMap == null && classAndBeanNameMap == null) {
            logger.error("传入的两个参数不能都为空");
            return false;
        }

        // 遍历类注解信息
        for (Map.Entry<String, Map<String, Map<String, BaseAnnotationAttribute>>> classAnnotationMapEntry : classAnnotationInfoMap.entrySet()) {
            String fullClassName = classAnnotationMapEntry.getKey();
            Map<String, Map<String, BaseAnnotationAttribute>> annotationMapOfClass = classAnnotationMapEntry.getValue();
            for (String springComponentAnnotations : CommonAnnotationConstants.SPRING_COMPONENT_ANNOTATIONS) {
                Map<String, BaseAnnotationAttribute> springComponentAnnotationMap = annotationMapOfClass.get(springComponentAnnotations);
                if (springComponentAnnotationMap == null) {
                    continue;
                }

                // 在当前类对应的注解中，找到Spring Component相关注解
                BaseAnnotationAttribute valueAttribute = springComponentAnnotationMap.get(CommonAnnotationConstants.SPRING_COMPONENT_ATTRIBUTE_NAME);
                if (valueAttribute == null) {
                    // Spring Component相关注解未指定value，则bean的名称为简单类名首字段小写
                    String simpleClassName = JACGUtil.getSimpleClassNameFromFull(fullClassName);
                    String firstLetterLowerClassName = JACGUtil.getFirstLetterLowerClassName(simpleClassName);
                    setBeanMap(firstLetterLowerClassName, fullClassName, beanNameAndClassMap, classAndBeanNameMap);
                    break;
                }

                // Spring Component相关注解有指定value，则作为bean的名称
                if (!(valueAttribute instanceof StringAnnotationAttribute)) {
                    logger.error("{} 类的 {} 注解 {} 属性类型非法 {}", fullClassName, springComponentAnnotations, CommonAnnotationConstants.SPRING_COMPONENT_ATTRIBUTE_NAME,
                            valueAttribute.getClass().getName());
                    return false;
                }

                String value = ((StringAnnotationAttribute) valueAttribute).getAttributeString();
                setBeanMap(value, fullClassName, beanNameAndClassMap, classAndBeanNameMap);
                break;
            }
        }

        return true;
    }

    private void setBeanMap(String beanName, String className, Map<String, String> beanNameAndClassMap, Map<String, String> classAndBeanNameMap) {
        if (beanNameAndClassMap != null) {
            beanNameAndClassMap.put(beanName, className);
        }

        if (classAndBeanNameMap != null) {
            classAndBeanNameMap.put(className, beanName);
        }
    }

    private AnnotationStorage() {
        throw new IllegalStateException("illegal");
    }
}
