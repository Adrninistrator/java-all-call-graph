package com.adrninistrator.jacg.handler.methodcall;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCallStaticFieldMCR;
import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData4MethodCallClassField;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.util.JACGSqlUtil;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2024/2/3
 * @description: 方法调用使用类的字段信息处理类，可用于获取枚举的使用情况
 */
public class MethodCallClassFieldHandler extends BaseHandler {

    public MethodCallClassFieldHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
    }

    public MethodCallClassFieldHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
    }

    /**
     * 查询指定的类的所有字段在方法调用中的使用情况
     *
     * @param queryStaticField true：查询静态字段 false：查询非静态字段
     * @param className        类名/枚举类名
     * @return
     */
    public List<BaseWriteDbData4MethodCallClassField> queryMethodCallClassField4Class(boolean queryStaticField, String className) {
        SqlKeyEnum sqlKeyEnum = queryStaticField ? SqlKeyEnum.MCSF_QUERY_BY_CLASS : SqlKeyEnum.MCNSF_QUERY_BY_CLASS;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            DbTableInfoEnum dbTableInfoEnum = chooseDbTableInfoEnum(queryStaticField);
            sql = "select " + JACGSqlUtil.getTableAllColumns(dbTableInfoEnum) +
                    " from " + dbTableInfoEnum.getTableName() +
                    " where " + DC.MCF_SIMPLE_CLASS_NAME + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryList(sql, BaseWriteDbData4MethodCallClassField.class, dbOperWrapper.querySimpleClassName(className));
    }

    /**
     * 查询指定的类的指定字段在方法调用中的使用情况
     *
     * @param queryStaticField true：查询静态字段 false：查询非静态字段
     * @param className        类名/枚举类名
     * @param fieldName        字段名/枚举常量名
     * @return
     */
    public List<BaseWriteDbData4MethodCallClassField> queryMethodCallClassField4ClassField(boolean queryStaticField, String className, String fieldName) {
        SqlKeyEnum sqlKeyEnum = queryStaticField ? SqlKeyEnum.MCSF_QUERY_BY_CLASS_FIELD : SqlKeyEnum.MCNSF_QUERY_BY_CLASS_FIELD;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            DbTableInfoEnum dbTableInfoEnum = chooseDbTableInfoEnum(queryStaticField);
            sql = "select " + JACGSqlUtil.getTableAllColumns(dbTableInfoEnum) +
                    " from " + dbTableInfoEnum.getTableName() +
                    " where " + DC.MCF_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.MCF_FIELD_NAME + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryList(sql, BaseWriteDbData4MethodCallClassField.class, dbOperWrapper.querySimpleClassName(className), fieldName);
    }

    /**
     * 查询方法调用中有使用的指定类中的字段名称或枚举常量列表（去重）
     * 查询静态字段时，支持查询枚举常量使用情况
     *
     * @param queryStaticField true：查询静态字段 false：查询非静态字段
     * @param className        类名
     * @return
     */
    public List<String> queryClassFieldNameInMCList(boolean queryStaticField, String className) {
        SqlKeyEnum sqlKeyEnum = queryStaticField ? SqlKeyEnum.MCSF_QUERY_CLASS_ALL_FIELD_NAME : SqlKeyEnum.MCNSF_QUERY_CLASS_ALL_FIELD_NAME;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            DbTableInfoEnum dbTableInfoEnum = chooseDbTableInfoEnum(queryStaticField);
            sql = "select distinct(" + DC.MCF_FIELD_NAME + ")" +
                    " from " + dbTableInfoEnum.getTableName() +
                    " where " + DC.MCF_SIMPLE_CLASS_NAME + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryListOneColumn(sql, String.class, dbOperWrapper.querySimpleClassName(className));
    }

    /**
     * 查询指定的类的静态字段方法调用返回值在方法调用中的使用情况（支持枚举常量值）
     *
     * @param className 类名/枚举类名
     * @return
     */
    public List<WriteDbData4MethodCallStaticFieldMCR> queryMethodCallStaticFieldMCR4Class(String className) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MCSFMCR_QUERY_BY_CLASS;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_METHOD_CALL_STATIC_FIELD_MCR) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL_STATIC_FIELD_MCR.getTableName() +
                    " where " + DC.MCSFMCR_SIMPLE_CLASS_NAME + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryList(sql, WriteDbData4MethodCallStaticFieldMCR.class, dbOperWrapper.querySimpleClassName(className));
    }

    /**
     * 查询指定的类的指定静态字段方法调用返回值在方法调用中的使用情况（支持枚举常量值）
     *
     * @param className 类名/枚举类名
     * @param fieldName 字段名/枚举常量名
     * @return
     */
    public List<WriteDbData4MethodCallStaticFieldMCR> queryMethodCallStaticFieldMCR4ClassField(String className, String fieldName) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MCSFMCR_QUERY_BY_CLASS_FIELD;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_METHOD_CALL_STATIC_FIELD_MCR) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL_STATIC_FIELD_MCR.getTableName() +
                    " where " + DC.MCSFMCR_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.MCSFMCR_FIELD_NAME + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryList(sql, WriteDbData4MethodCallStaticFieldMCR.class, dbOperWrapper.querySimpleClassName(className), fieldName);
    }

    /**
     * 查询指定方法调用（的被调用对象或参数）使用的静态字段方法调用返回值
     *
     * @param callId
     * @param objArgSeq
     * @return
     */
    public List<WriteDbData4MethodCallStaticFieldMCR> queryMethodCallStaticFieldMCR4MethodCall(int callId, int objArgSeq) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MCSFMCR_QUERY_BY_METHOD_CALL;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_METHOD_CALL_STATIC_FIELD_MCR) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL_STATIC_FIELD_MCR.getTableName() +
                    " where " + DC.MCF_CALL_ID + " = ?" +
                    " and " + DC.MCF_OBJ_ARGS_SEQ + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryList(sql, WriteDbData4MethodCallStaticFieldMCR.class, callId, objArgSeq);
    }

    private DbTableInfoEnum chooseDbTableInfoEnum(boolean queryStaticField) {
        return queryStaticField ? DbTableInfoEnum.DTIE_METHOD_CALL_STATIC_FIELD : DbTableInfoEnum.DTIE_METHOD_CALL_NON_STATIC_FIELD;
    }
}
