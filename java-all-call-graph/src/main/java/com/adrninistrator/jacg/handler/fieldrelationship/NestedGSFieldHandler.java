package com.adrninistrator.jacg.handler.fieldrelationship;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4FieldGenericsType;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4GetMethod;
import com.adrninistrator.jacg.handler.annotation.AnnotationHandler;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.handler.dto.field.CustomFieldType;
import com.adrninistrator.jacg.handler.dto.field.NestedFieldTopClassInfo;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.dto.counter.JavaCG2Counter;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2023/9/27
 * @description: 嵌套的字段处理类
 * 说明： 可修改为从field_info表查询，根据exists_get_method、exists_set_method字段判断是否存在get/set方法
 */
public class NestedGSFieldHandler extends BaseHandler {
    private static final Logger logger = LoggerFactory.getLogger(NestedGSFieldHandler.class);

    private final AnnotationHandler annotationHandler;

    public NestedGSFieldHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
        annotationHandler = new AnnotationHandler(dbOperWrapper);
    }

    public NestedGSFieldHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
        annotationHandler = new AnnotationHandler(dbOperWrapper);
    }

    /**
     * 查询仅在一个类中被使用的嵌套类型的顶层类型信息
     *
     * @param className             被使用的嵌套类型
     * @param uniqueCustomFieldType 仅在一个类中被使用的嵌套类型
     * @return null: 当前类型不存在仅在一个类中被使用的嵌套类型的顶层类型 非null: 当前类型存在仅在一个类中被使用的嵌套类型的顶层类型
     */
    public NestedFieldTopClassInfo queryUniqueNestedFieldTopClassInfo(String className, CustomFieldType uniqueCustomFieldType) {
        if (className == null || uniqueCustomFieldType == null) {
            throw new JavaCG2RuntimeException("参数不允许为空");
        }
        // 判断仅在一个类中被使用的嵌套类型是否有被初始化
        if (!uniqueCustomFieldType.isInited()) {
            // 仅在一个类中被使用的嵌套类型未被初始化
            // 查询在get方法中，返回对象属于自定义类型，且仅在一个类中被使用的类型列表
            List<String> uniqueGetCustomFieldList = queryUniqueGetCustomFieldList();
            uniqueCustomFieldType.setCustomFieldTypeSet(new HashSet<>(uniqueGetCustomFieldList));
            uniqueCustomFieldType.setInited(true);
        }
        Set<String> uniqueGetCustomFieldSet = uniqueCustomFieldType.getCustomFieldTypeSet();
        if (!uniqueGetCustomFieldSet.contains(className)) {
            // 当前类型不存在仅在一个类中被使用的嵌套类型的顶层类型
            return null;
        }
        String currentClassName = className;
        List<String> currentClassNestedFieldNameList = new ArrayList<>();
        List<String> currentClassNestedFieldNameJsonAliasList = new ArrayList<>();
        JavaCG2Counter jsonAliasCounter = new JavaCG2Counter(0);
        List<String> allClassNameList = new ArrayList<>();
        while (true) {
            if (allClassNameList.contains(currentClassName)) {
                logger.error("当前处理的涉及嵌套的字段相关类出现循环引用\n{}\n{}", StringUtils.join(allClassNameList, "\n"), currentClassName);
                return null;
            }
            allClassNameList.add(currentClassName);
            // 查询指定类作为字段类型所在的上层类的类型
            String nestedFieldUpperClassName = queryNestedFieldUpperClassName(currentClassName, currentClassNestedFieldNameList, currentClassNestedFieldNameJsonAliasList,
                    jsonAliasCounter);
            if (nestedFieldUpperClassName == null) {
                // 指定类作为字段类型所在的上层类有多个
                return null;
            }
            if (nestedFieldUpperClassName.isEmpty()) {
                // 指定类作为字段类型所在的上层类不存在
                break;
            }
            currentClassName = nestedFieldUpperClassName;
        }
        return new NestedFieldTopClassInfo(currentClassName, currentClassNestedFieldNameList, currentClassNestedFieldNameJsonAliasList, jsonAliasCounter.getCount() > 0);
    }

    /**
     * 查询指定类作为字段类型所在的上层类的类型
     *
     * @param className                                指定的类名
     * @param currentClassNestedFieldNameList          当前类在顶层类型中的嵌套的字段名称列表
     * @param currentClassNestedFieldNameJsonAliasList 当前类在顶层类型中的嵌套的字段名称列表，通过@JsonProperty注解指定别名
     * @param jsonAliasCounter                         通过@JsonProperty注解指定别名的数量
     * @return null: 指定类作为字段类型所在的上层类有多个 "": 指定类作为字段类型所在的上层类不存在 非null且非"": 指定类作为字段类型所在的上层类有且仅有一个
     */
    private String queryNestedFieldUpperClassName(String className,
                                                  List<String> currentClassNestedFieldNameList,
                                                  List<String> currentClassNestedFieldNameJsonAliasList,
                                                  JavaCG2Counter jsonAliasCounter) {
        // 记录指定类仅在一个类中被使用的嵌套类型的上层类的类型
        String nestedFieldUpperClassName = null;
        // 记录指定类在上层类中的字段名
        String fieldName = null;
        // 首先从get方法表中查询指定类以非泛型类型被嵌套使用的上层类的类型
        String simpleClassName = dbOperWrapper.querySimpleClassName(className);
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.GM_QUERY_UPPER_NESTED_FIELD_TYPE;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.joinColumns(DC.GSM_CLASS_NAME, DC.GSM_FIELD_NAME) +
                    " from " + DbTableInfoEnum.DTIE_GET_METHOD.getTableName() +
                    " where " + DC.GSM_FIELD_CATEGORY + " = ?" +
                    " and " + DC.GSM_SIMPLE_FIELD_TYPE + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        List<WriteDbData4GetMethod> getMethodList = dbOperator.queryList(sql, WriteDbData4GetMethod.class, JavaCG2Constants.FILE_KEY_CATEGORY_CUSTOM, simpleClassName);
        if (!JavaCG2Util.isCollectionEmpty(getMethodList)) {
            if (getMethodList.size() > 1) {
                // 指定类作为字段类型所在的上层类有多个
                return null;
            }
            nestedFieldUpperClassName = getMethodList.get(0).getClassName();
            fieldName = getMethodList.get(0).getFieldName();
        } else {
            // 再从get方法表、非静态字段中涉及的泛型类型表中查询指定类被嵌套使用的上层类的类型
            sqlKeyEnum = SqlKeyEnum.GM_FGT_QUERY_UPPER_NESTED_FIELD_TYPE;
            sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
            if (sql == null) {
                sql = "select " + JACGSqlUtil.joinColumns("fgt." + DC.FGT_CLASS_NAME, "fgt." + DC.FGT_FIELD_NAME) +
                        " from " + DbTableInfoEnum.DTIE_GET_METHOD.getTableName() + " as gm, " + DbTableInfoEnum.DTIE_FIELD_GENERICS_TYPE.getTableName() + " as fgt" +
                        " where gm." + DC.GSM_SIMPLE_CLASS_NAME + " = fgt." + DC.FGT_SIMPLE_CLASS_NAME +
                        " and gm." + DC.GSM_FIELD_NAME + " = fgt." + DC.GSM_FIELD_NAME +
                        " and gm." + DC.GSM_FIELD_CATEGORY + " = ?" +
                        " and fgt." + DC.FGT_GENERICS_CATEGORY + " = ?" +
                        " and fgt." + DC.FGT_SIMPLE_GENERICS_TYPE + " = ?";
                sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
            }
            List<WriteDbData4FieldGenericsType> fieldGenericsTypeList = dbOperator.queryList(sql, WriteDbData4FieldGenericsType.class,
                    JavaCG2Constants.FILE_KEY_CATEGORY_GENERICS_CUSTOM, JavaCG2Constants.FILE_KEY_CATEGORY_CUSTOM, simpleClassName);
            if (!JavaCG2Util.isCollectionEmpty(fieldGenericsTypeList)) {
                if (fieldGenericsTypeList.size() > 1) {
                    // 指定类作为字段类型所在的上层类有多个
                    return null;
                }
                nestedFieldUpperClassName = fieldGenericsTypeList.get(0).getClassName();
                fieldName = fieldGenericsTypeList.get(0).getFieldName();
            }
        }
        if (nestedFieldUpperClassName == null) {
            // 未查询到上层类信息，返回""
            return "";
        }
        // 查询到上层类信息，列表需要逆序添加，下同
        currentClassNestedFieldNameList.add(0, fieldName);
        // 获取指定类字段使用@JsonProperty注解的JSON别名
        String fieldJsonPropertyValue = annotationHandler.queryFieldJsonPropertyValue(nestedFieldUpperClassName, fieldName);
        if (fieldJsonPropertyValue != null) {
            currentClassNestedFieldNameJsonAliasList.add(0, fieldJsonPropertyValue);
            jsonAliasCounter.addAndGet();
        } else {
            currentClassNestedFieldNameJsonAliasList.add(0, fieldName);
        }
        return nestedFieldUpperClassName;
    }

    /**
     * 查询在get方法中，返回对象属于自定义类型，且仅在一个类中被使用的类型列表
     *
     * @return
     */
    private List<String> queryUniqueGetCustomFieldList() {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.GM_FGT_QUERY_UNIQUE_CUSTOM_FIELD;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select r." + DC.GSM_FIELD_TYPE + " from" +
                    " (" +
                    " select " + DC.GSM_FIELD_TYPE +
                    " from " + DbTableInfoEnum.DTIE_GET_METHOD.getTableName() +
                    " where " + DC.GSM_FIELD_CATEGORY + " = ?" +
                    " union all" +
                    " select fgt." + DC.FGT_GENERICS_TYPE + " as " + DC.GSM_FIELD_TYPE +
                    " from " + DbTableInfoEnum.DTIE_GET_METHOD.getTableName() + " as gm, " + DbTableInfoEnum.DTIE_FIELD_GENERICS_TYPE.getTableName() + " as fgt" +
                    " where gm." + DC.GSM_SIMPLE_CLASS_NAME + " = fgt." + DC.FGT_SIMPLE_CLASS_NAME +
                    " and gm." + DC.GSM_FIELD_NAME + " = fgt." + DC.FGT_FIELD_NAME +
                    " and gm." + DC.GSM_FIELD_CATEGORY + " = ?" +
                    " and fgt." + DC.FGT_GENERICS_CATEGORY + " = ?" +
                    " ) as r" +
                    " group by r." + DC.GSM_FIELD_TYPE +
                    " having count(r." + DC.GSM_FIELD_TYPE + ") = 1";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryListOneColumn(sql, String.class, JavaCG2Constants.FILE_KEY_CATEGORY_CUSTOM, JavaCG2Constants.FILE_KEY_CATEGORY_GENERICS_CUSTOM,
                JavaCG2Constants.FILE_KEY_CATEGORY_CUSTOM);
    }
}
