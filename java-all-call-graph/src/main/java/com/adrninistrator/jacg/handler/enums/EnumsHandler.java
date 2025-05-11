package com.adrninistrator.jacg.handler.enums;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.JACGCommonNameConstants;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.handler.method.MethodInfoHandler;
import org.apache.commons.lang3.StringUtils;

/**
 * @author adrninistrator
 * @date 2025/1/18
 * @description: 枚举信息处理类
 */
public class EnumsHandler extends BaseHandler {

    private final MethodInfoHandler methodInfoHandler;

    public EnumsHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
        methodInfoHandler = new MethodInfoHandler(dbOperWrapper);
    }

    public EnumsHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
        methodInfoHandler = new MethodInfoHandler(dbOperWrapper);
    }

    /**
     * 查询枚举常量字段在初始化时的值
     *
     * @param enumClassName 枚举类名
     * @param enumConstName 枚举常量名称
     * @param fieldName     字段名称
     * @return
     */
    public String queryEnumConstantFieldValue(String enumClassName, String enumConstName, String fieldName) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.ENUM_QUERY_FIELD_VALUE;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select eiai." + DC.EIAI_FIELD_VALUE +
                    " from " + DbTableInfoEnum.DTIE_ENUM_INIT_ARG_FIELD.getTableName() + " as eiaf, " +
                    DbTableInfoEnum.DTIE_ENUM_INIT_ASSIGN_INFO.getTableName() + " as eiai" +
                    " where eiaf." + DC.EIAF_SIMPLE_CLASS_NAME + " = eiai." + DC.EIAI_SIMPLE_CLASS_NAME +
                    " and eiaf." + DC.EIAF_ARG_SEQ + " = eiai." + DC.EIAI_ARG_SEQ +
                    " and eiaf." + DC.EIAF_SIMPLE_CLASS_NAME + " = ?" +
                    " and eiai." + DC.EIAI_CONST_NAME + " = ?" +
                    " and eiaf." + DC.EIAF_FIELD_NAME + " = ?" +
                    " limit 1";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryObjectOneColumn(sql, String.class, dbOperWrapper.querySimpleClassName(enumClassName), enumConstName, fieldName);
    }

    /**
     * 查询枚举常量方法返回值
     *
     * @param enumClassName
     * @param enumConstName
     * @param enumFullMethod
     * @param enumReturnType
     * @return
     */
    public String queryEnumConstantFieldMethodReturnValue(String enumClassName, String enumConstName, String enumFullMethod, String enumReturnType) {
        // 判断是否返回了枚举的name()方法
        if (enumFullMethod.endsWith(JACGCommonNameConstants.ENUM_METHOD_NAME)) {
            return enumConstName;
        }

        String methodReturnFieldName = methodInfoHandler.queryMethodReturnFieldName(enumFullMethod, enumReturnType);
        if (StringUtils.isBlank(methodReturnFieldName)) {
            return null;
        }
        return queryEnumConstantFieldValue(enumClassName, enumConstName, methodReturnFieldName);
    }
}
