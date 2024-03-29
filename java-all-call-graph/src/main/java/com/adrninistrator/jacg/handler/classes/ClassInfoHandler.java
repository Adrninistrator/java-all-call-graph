package com.adrninistrator.jacg.handler.classes;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.javacg.dto.access_flag.JavaCGAccessFlags;

/**
 * @author adrninistrator
 * @date 2023/4/13
 * @description: 查询类的信息处理类
 */
public class ClassInfoHandler extends BaseHandler {
    public ClassInfoHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
    }

    public ClassInfoHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
    }

    /**
     * 根据完整类名查询类的JavaCGAccessFlags对象，可调用方法判断类的属性
     *
     * @param className 完整类名
     * @return 可能为null
     */
    public JavaCGAccessFlags queryClassJavaCGAccessFlags(String className) {
        Integer accessFlags = queryClassAccessFlag(className);
        if (accessFlags == null) {
            return null;
        }
        return new JavaCGAccessFlags(accessFlags);
    }

    /**
     * 根据完整类名查询类的access_flags
     *
     * @param className 完整类名
     * @return 可能为null
     */
    public Integer queryClassAccessFlag(String className) {
        String simpleClassName = dbOperWrapper.getSimpleClassName(className);
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.CI_QUERY_ACCESS_FLAGS;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + DC.CI_ACCESS_FLAGS +
                    " from " + DbTableInfoEnum.DTIE_CLASS_INFO.getTableName() +
                    " where " + DC.CSEI1_SIMPLE_CLASS_NAME + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryObjectOneColumn(sql, Integer.class, simpleClassName);
    }
}
