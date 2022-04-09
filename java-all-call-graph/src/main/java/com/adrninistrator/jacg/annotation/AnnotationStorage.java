package com.adrninistrator.jacg.annotation;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.dboper.DbOperator;
import com.adrninistrator.jacg.dto.annotation.AnnotationInfo4Method;
import com.adrninistrator.jacg.dto.annotation.AnnotationInfo4Read;
import com.adrninistrator.jacg.extensions.util.JsonUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.fasterxml.jackson.core.type.TypeReference;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2022/4/9
 * @description:
 */
public class AnnotationStorage {
    private static final Logger logger = LoggerFactory.getLogger(AnnotationStorage.class);

    /*
        保存类注解信息
        key 完整类名
        value 注解信息
     */
    private static Map<String, List<AnnotationInfo4Read>> CLASS_ANNOTATION_INFO_MAP = new HashMap<>(100);

    /*
        保存方法注解信息
        key 方法完整名称HASH+长度
        value 注解信息
     */
    private static Map<String, AnnotationInfo4Method> METHOD_ANNOTATION_INFO_MAP = new HashMap<>(100);

    // 初始化
    public static boolean init(DbOperator dbOperator, String appName) {
        // 从数据库查询类注解信息
        if (!queryClassAnnotationInfo(dbOperator, appName)) {
            return false;
        }

        // 从数据库查询方法注解信息
        return queryMethodAnnotationInfo(dbOperator, appName);
    }

    // 从数据库查询类注解信息
    private static boolean queryClassAnnotationInfo(DbOperator dbOperator, String appName) {
        logger.info("从数据库查询类注解信息");
        String columns = StringUtils.join(JACGConstants.TABLE_COLUMNS_CLASS_ANNOTATION, JACGConstants.FLAG_COMMA_WITH_SPACE);
        // 以下查询需要按照类名及注解类名进行排序，使后续处理时是有序的
        String sql = "select " + columns + " from " + JACGConstants.TABLE_PREFIX_CLASS_ANNOTATION + appName +
                " order by " + DC.CA_FULL_CLASS_NAME + ", " + DC.MA_ANNOTATION_NAME;
        List<Map<String, Object>> list = dbOperator.queryList(sql, null);
        if (list == null) {
            return false;
        }

        if (list.isEmpty()) {
            return true;
        }

        String lastFullClassName = "";
        for (Map<String, Object> map : list) {
            String fullClassName = (String) map.get(DC.CA_FULL_CLASS_NAME);
            String annotationName = (String) map.get(DC.CA_ANNOTATION_NAME);
            String annotationAttributes = (String) map.get(DC.CA_ANNOTATION_ATTRIBUTES);

            AnnotationInfo4Read annotationInfo4Read = new AnnotationInfo4Read();
            annotationInfo4Read.setAnnotationName(annotationName);
            annotationInfo4Read.setAnnotationAttributeMap(JsonUtil.getObjFromJsonStr(annotationAttributes, new TypeReference<Map<String, String>>() {
            }));

            if (lastFullClassName.equals(fullClassName)) {
                // 上次处理的与当前处理的是同一个类
                List<AnnotationInfo4Read> annotationInfo4ReadList = CLASS_ANNOTATION_INFO_MAP.get(fullClassName);
                annotationInfo4ReadList.add(annotationInfo4Read);
            } else {
                // 上次处理的与当前处理的不是同一个类
                List<AnnotationInfo4Read> annotationInfo4ReadList = new ArrayList<>();
                annotationInfo4ReadList.add(annotationInfo4Read);

                CLASS_ANNOTATION_INFO_MAP.put(fullClassName, annotationInfo4ReadList);

                lastFullClassName = fullClassName;
            }
        }
        logger.info("查询到类注解信息数量 {}", CLASS_ANNOTATION_INFO_MAP.size());
        return true;
    }

    // 从数据库查询方法注解信息
    private static boolean queryMethodAnnotationInfo(DbOperator dbOperator, String appName) {
        logger.info("从数据库查询方法注解信息");
        String columns = StringUtils.join(JACGConstants.TABLE_COLUMNS_METHOD_ANNOTATION, JACGConstants.FLAG_COMMA_WITH_SPACE);
        // 以下查询需要按照方法HASH及注解类名进行排序，使后续处理时是有序的
        String sql = "select " + columns + " from " + JACGConstants.TABLE_PREFIX_METHOD_ANNOTATION + appName +
                " order by " + DC.MA_METHOD_HASH + ", " + DC.MA_ANNOTATION_NAME;
        List<Map<String, Object>> list = dbOperator.queryList(sql, null);
        if (list == null) {
            return false;
        }

        if (list.isEmpty()) {
            return true;
        }

        String lastMethodHash = "";
        for (Map<String, Object> map : list) {
            String methodHash = (String) map.get(DC.MA_METHOD_HASH);
            String annotationName = (String) map.get(DC.MA_ANNOTATION_NAME);
            String annotationAttributes = (String) map.get(DC.MA_ANNOTATION_ATTRIBUTES);
            String fullMethod = (String) map.get(DC.MA_FULL_METHOD);

            AnnotationInfo4Read annotationInfo4Read = new AnnotationInfo4Read();
            annotationInfo4Read.setAnnotationName(annotationName);
            annotationInfo4Read.setAnnotationAttributeMap(JsonUtil.getObjFromJsonStr(annotationAttributes, new TypeReference<Map<String, String>>() {
            }));

            if (lastMethodHash.equals(methodHash)) {
                // 上次处理的与当前处理的是同一个方法
                AnnotationInfo4Method annotationInfo4Method = METHOD_ANNOTATION_INFO_MAP.get(methodHash);
                annotationInfo4Method.getAnnotationInfo4ReadList().add(annotationInfo4Read);
            } else {
                // 上次处理的与当前处理的不是同一个方法
                List<AnnotationInfo4Read> annotationInfo4ReadList = new ArrayList<>();
                annotationInfo4ReadList.add(annotationInfo4Read);

                AnnotationInfo4Method annotationInfo4Method = new AnnotationInfo4Method();
                annotationInfo4Method.setFullMethod(fullMethod);
                annotationInfo4Method.setFullClassName(JACGUtil.getFullClassNameFromMethod(fullMethod));
                annotationInfo4Method.setAnnotationInfo4ReadList(annotationInfo4ReadList);
                METHOD_ANNOTATION_INFO_MAP.put(methodHash, annotationInfo4Method);

                lastMethodHash = methodHash;
            }
        }
        logger.info("查询到方法注解信息数量 {}", METHOD_ANNOTATION_INFO_MAP.size());
        return true;
    }

    // 根据完整类名获取对应的注解信息
    public static List<AnnotationInfo4Read> getAnnotationInfo4Class(String fullClassName) {
        return CLASS_ANNOTATION_INFO_MAP.get(fullClassName);
    }

    // 根据完整方法HASH+长度获取对应的注解信息
    public static AnnotationInfo4Method getAnnotationInfo4Method(String methodHash) {
        return METHOD_ANNOTATION_INFO_MAP.get(methodHash);
    }

    private AnnotationStorage() {
        throw new IllegalStateException("illegal");
    }
}
