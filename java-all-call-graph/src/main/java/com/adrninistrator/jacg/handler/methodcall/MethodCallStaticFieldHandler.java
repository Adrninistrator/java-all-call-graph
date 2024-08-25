package com.adrninistrator.jacg.handler.methodcall;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCall;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCallStaticField;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.handler.dto.methodcall.MethodCallWithStaticField;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.javacg.util.JavaCGUtil;
import org.apache.commons.lang3.ArrayUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2024/2/3
 * @description: 方法调用使用静态字段信息处理类，可用于获取枚举的使用情况
 */
public class MethodCallStaticFieldHandler extends BaseHandler {
    private MethodCallHandler methodCallHandler;

    public MethodCallStaticFieldHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
        methodCallHandler = new MethodCallHandler(dbOperWrapper);
    }

    public MethodCallStaticFieldHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
        methodCallHandler = new MethodCallHandler(dbOperWrapper);
    }

    /**
     * 查询在方法调用中使用了指定类中的静态字段的信息，包含对应的方法调用
     * 可用于获取枚举的使用情况
     *
     * @param className  类名/枚举类名
     * @param fieldNames 字段名/枚举字段名
     * @return
     */
    public List<MethodCallWithStaticField> queryMethodCallWithStaticFieldList(String className, String... fieldNames) {
        List<MethodCallWithStaticField> methodCallWithStaticFieldList = new ArrayList<>();
        List<String> fieldNameList;
        if (ArrayUtils.isEmpty(fieldNames)) {
            // 查询方法调用中有使用的指定类中的静态字段名称列表（去重）
            fieldNameList = queryClassStaticFieldNameList(className);
        } else {
            fieldNameList = Arrays.asList(fieldNames);
        }
        for (String fieldName : fieldNameList) {
            // 查询指定类的指定字段在方法调用中的使用情况
            queryMCSF4ClassField(methodCallWithStaticFieldList, className, fieldName);
        }
        return methodCallWithStaticFieldList;
    }

    // 查询指定类的指定字段在方法调用中的使用情况
    private void queryMCSF4ClassField(List<MethodCallWithStaticField> methodCallWithStaticFieldList, String className, String fieldName) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MCSF_QUERY_BY_CLASS_FIELD;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_METHOD_CALL_STATIC_FIELD) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL_STATIC_FIELD.getTableName() +
                    " where " + DC.MCSF_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.MCSF_FIELD_NAME + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        List<WriteDbData4MethodCallStaticField> methodCallStaticFieldList = dbOperator.queryList(sql, WriteDbData4MethodCallStaticField.class,
                dbOperWrapper.querySimpleClassName(className), fieldName);
        if (JavaCGUtil.isCollectionEmpty(methodCallStaticFieldList)) {
            return;
        }

        for (WriteDbData4MethodCallStaticField methodCallStaticField : methodCallStaticFieldList) {
            WriteDbData4MethodCall methodCall = methodCallHandler.queryMethodCallByCallId(methodCallStaticField.getCallId());
            MethodCallWithStaticField methodCallWithStaticField = new MethodCallWithStaticField(methodCall, methodCallStaticField);
            methodCallWithStaticFieldList.add(methodCallWithStaticField);
        }
    }

    // 查询方法调用中有使用的指定类中的静态字段名称列表（去重）
    private List<String> queryClassStaticFieldNameList(String className) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MCSF_QUERY_CLASS_ALL_FIELD_NAME;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select distinct(" + DC.MCSF_FIELD_NAME + ")" +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL_STATIC_FIELD.getTableName() +
                    " where " + DC.MCSF_SIMPLE_CLASS_NAME + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        return dbOperator.queryListOneColumn(sql, String.class, dbOperWrapper.querySimpleClassName(className));
    }
}
