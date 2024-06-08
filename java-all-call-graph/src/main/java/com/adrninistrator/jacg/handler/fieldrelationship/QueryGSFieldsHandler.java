package com.adrninistrator.jacg.handler.fieldrelationship;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4FieldGenericsType;
import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData4GetSetMethod;
import com.adrninistrator.jacg.handler.annotation.AnnotationHandler;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.handler.dto.field.JACGFieldInfo;
import com.adrninistrator.jacg.handler.field.FieldInfoHandler;
import com.adrninistrator.javacg.common.JavaCGConstants;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2023/8/9
 * @description: 查询字段的处理类
 */
public class QueryGSFieldsHandler extends BaseHandler {

    private static final Logger logger = LoggerFactory.getLogger(QueryGSFieldsHandler.class);

    private final GetSetMethodHandler getSetMethodHandler;

    private final AnnotationHandler annotationHandler;

    private final FieldInfoHandler fieldInfoHandler;

    public QueryGSFieldsHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
        getSetMethodHandler = new GetSetMethodHandler(dbOperWrapper);
        annotationHandler = new AnnotationHandler(dbOperWrapper);
        fieldInfoHandler = new FieldInfoHandler(dbOperWrapper);
    }

    public QueryGSFieldsHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
        getSetMethodHandler = new GetSetMethodHandler(dbOperWrapper);
        annotationHandler = new AnnotationHandler(dbOperWrapper);
        fieldInfoHandler = new FieldInfoHandler(dbOperWrapper);
    }

    /**
     * 查询指定类中get/set方法对应的全部字段，包含自定义类型中的字段
     *
     * @param queryGetMethod true: 查询get方法对应的字段 false: 查询set方法对应的字段
     * @param className      类名
     * @return
     */
    public List<JACGFieldInfo> queryAllFieldInfoList(boolean queryGetMethod, String className) {
        List<String> upperFieldUsedNameList = new ArrayList<>();
        List<JACGFieldInfo> allFieldInfoList = new ArrayList<>();
        Set<String> handledClassNameSet = new HashSet<>();
        doQueryAllFieldInfoList(allFieldInfoList, queryGetMethod, className, upperFieldUsedNameList, handledClassNameSet);
        return allFieldInfoList;
    }

    // 查询指定类中get/set方法对应的全部字段，包含自定义类型中的字段，递归处理
    private void doQueryAllFieldInfoList(List<JACGFieldInfo> allFieldInfoList, boolean queryGetMethod, String className, List<String> upperFieldUsedNameList,
                                         Set<String> handledClassNameSet) {
        // 判断当前类是否有处理过，避免死循环
        if (!handledClassNameSet.add(className)) {
            logger.info("当前类已经处理过，出现死循环 {}", className);
            return;
        }

        // 根据类名，查询对应的get/set方法
        List<BaseWriteDbData4GetSetMethod> getSetMethodList = getSetMethodHandler.queryGetSetMethodByClassName(queryGetMethod, className);
        if (getSetMethodList == null) {
            return;
        }

        for (BaseWriteDbData4GetSetMethod getSetMethod : getSetMethodList) {
            String fieldName = getSetMethod.getFieldName();
            String jsonPropertyValue = annotationHandler.queryFieldJsonPropertyValue(className, fieldName);
            // 获取使用的字段的名称
            String fieldUsedName = StringUtils.isBlank(jsonPropertyValue) ? fieldName : jsonPropertyValue;
            if (StringUtils.equalsAny(getSetMethod.getFieldCategory(), JavaCGConstants.FILE_KEY_CATEGORY_JDK, JavaCGConstants.FILE_KEY_CATEGORY_GENERICS_JDK)) {
                // 字段类型为JDK中的类，或集合的泛型类型为JDK中的类，直接添加
                String fieldShowName = getFieldShowName(fieldUsedName, upperFieldUsedNameList);
                allFieldInfoList.add(new JACGFieldInfo(fieldName, getSetMethod.getFieldType(), fieldShowName, jsonPropertyValue, className));
                continue;
            }

            List<String> newUpperFieldUsedNameList = new ArrayList<>(upperFieldUsedNameList);
            newUpperFieldUsedNameList.add(fieldUsedName);
            if (JavaCGConstants.FILE_KEY_CATEGORY_CUSTOM.equals(getSetMethod.getFieldCategory())) {
                // 字段类型为自定义类型，递归处理
                doQueryAllFieldInfoList(allFieldInfoList, queryGetMethod, getSetMethod.getFieldType(), newUpperFieldUsedNameList, handledClassNameSet);
                continue;
            }

            // 字段集合泛型类型为自定义类型，查询泛型中的类型
            List<WriteDbData4FieldGenericsType> fieldGenericsTypeList = fieldInfoHandler.queryFieldGenericsTypeByClassFieldName(className, fieldName);
            if (fieldGenericsTypeList == null) {
                continue;
            }
            for (WriteDbData4FieldGenericsType fieldGenericsType : fieldGenericsTypeList) {
                // 递归处理
                doQueryAllFieldInfoList(allFieldInfoList, queryGetMethod, fieldGenericsType.getFieldGenericsType(), newUpperFieldUsedNameList, handledClassNameSet);
            }
        }
    }

    /**
     * 查询指定类中get/set方法对应的字段自定义类型
     *
     * @param queryGetMethod true: 查询get方法对应的字段 false: 查询set方法对应的字段
     * @param className      类名
     * @return
     */
    public List<String> queryCustomFieldTypeList(boolean queryGetMethod, String className) {
        List<String> customFieldTypeList = new ArrayList<>();
        Set<String> handledClassNameSet = new HashSet<>();
        doQueryCustomFieldTypeList(customFieldTypeList, queryGetMethod, className, handledClassNameSet);
        return customFieldTypeList;
    }

    // 查询指定类中get/set方法对应的字段自定义类型，递归处理
    public void doQueryCustomFieldTypeList(List<String> customFieldTypeList, boolean queryGetMethod, String className, Set<String> handledClassNameSet) {
        // 判断当前类是否有处理过，避免死循环
        if (!handledClassNameSet.add(className)) {
            logger.info("当前类已经处理过，出现死循环 {}", className);
            return;
        }

        // 根据类名，查询对应的get/set方法
        List<BaseWriteDbData4GetSetMethod> getSetMethodList = getSetMethodHandler.queryGetSetMethodByClassName(queryGetMethod, className);
        if (getSetMethodList == null) {
            return;
        }

        for (BaseWriteDbData4GetSetMethod getSetMethod : getSetMethodList) {
            if (JavaCGConstants.FILE_KEY_CATEGORY_CUSTOM.equals(getSetMethod.getFieldCategory())) {
                // 字段类型为自定义类型
                customFieldTypeList.add(getSetMethod.getFieldType());
                // 递归处理
                doQueryCustomFieldTypeList(customFieldTypeList, queryGetMethod, getSetMethod.getFieldType(), handledClassNameSet);
                continue;
            }

            // 字段集合泛型类型为自定义类型，查询泛型中的类型
            if (JavaCGConstants.FILE_KEY_CATEGORY_GENERICS_CUSTOM.equals(getSetMethod.getFieldCategory())) {
                List<WriteDbData4FieldGenericsType> fieldGenericsTypeList = fieldInfoHandler.queryFieldGenericsTypeByClassFieldName(className, getSetMethod.getFieldName());
                if (fieldGenericsTypeList == null) {
                    return;
                }
                for (WriteDbData4FieldGenericsType fieldGenericsType : fieldGenericsTypeList) {
                    // 递归处理
                    doQueryCustomFieldTypeList(customFieldTypeList, queryGetMethod, fieldGenericsType.getFieldGenericsType(), handledClassNameSet);
                }
            }
        }
    }

    /**
     * 查询使用了指定类型字段对应的类名，包含直接使用以及在集合的泛型类型中使用
     *
     * @param queryGetMethod true: 查询get方法 false: 查询set方法
     * @param fieldType      字段类型
     * @return
     */
    public List<String> queryClassesByFieldType(boolean queryGetMethod, String fieldType) {
        Set<String> classNameSet = new HashSet<>();

        // 查询直接使用指定类型字段的类
        List<String> classNameList1 = getSetMethodHandler.queryClassesByFieldType(queryGetMethod, fieldType);
        if (classNameList1 != null) {
            classNameSet.addAll(classNameList1);
        }

        // 查询在集合的泛型类型中使用指定类型字段的类
        List<String> classNameList2 = fieldInfoHandler.queryFieldGenericsTypeByType(fieldType);
        if (classNameList2 != null) {
            classNameSet.addAll(classNameList2);
        }
        List<String> classNameList = new ArrayList<>(classNameSet);
        Collections.sort(classNameList);
        return classNameList;
    }

    /**
     * 获取字段显示名称，使用.代表嵌套类型中的字段，支持@JsonProperty注解中的名称
     *
     * @param fieldUsedName
     * @param upperFieldUsedNameList
     * @return
     */
    private String getFieldShowName(String fieldUsedName, List<String> upperFieldUsedNameList) {
        if (upperFieldUsedNameList.isEmpty()) {
            return fieldUsedName;
        }
        List<String> newList = new ArrayList<>(upperFieldUsedNameList);
        newList.add(fieldUsedName);
        return StringUtils.join(newList, JavaCGConstants.FLAG_DOT);
    }
}