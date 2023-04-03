package com.adrninistrator.jacg.handler.method;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.handler.base.BaseHandler;

import java.util.Map;

/**
 * @author adrninistrator
 * @date 2023/3/26
 * @description: 方法处理类
 */
public class MethodInfoHandler extends BaseHandler {
    public MethodInfoHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
    }

    public MethodInfoHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
    }

    /**
     * 根据完整类名与方法代码行号查询对应的完整方法
     *
     * @param className
     * @param lineNumber
     * @return null: 未查询到
     */
    public String getFullMethodByClassLine(String className, int lineNumber) {
        String simpleClassName = dbOperWrapper.getSimpleClassName(className);
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MLN_QUERY_METHOD;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + DC.MLN_FULL_METHOD +
                    " from " + DbTableInfoEnum.DTIE_METHOD_LINE_NUMBER.getTableName() +
                    " where " + DC.MLN_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.MLN_MIN_LINE_NUMBER + " <= ?" +
                    " and " + DC.MLN_MAX_LINE_NUMBER + " >= ?" +
                    " limit 1";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        Map<String, Object> map = dbOperator.queryOneRow(sql, new Object[]{simpleClassName, lineNumber, lineNumber});
        if (map == null) {
            return null;
        }
        return (String) map.get(DC.MLN_FULL_METHOD);
    }
}
