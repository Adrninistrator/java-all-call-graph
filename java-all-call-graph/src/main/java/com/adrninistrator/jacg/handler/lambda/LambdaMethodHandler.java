package com.adrninistrator.jacg.handler.lambda;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4LambdaMethodInfo;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.util.JACGSqlUtil;

/**
 * @author adrninistrator
 * @date 2024/4/22
 * @description: Lambda方法处理类
 */
public class LambdaMethodHandler extends BaseHandler {
    public LambdaMethodHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
    }

    public LambdaMethodHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
    }

    /**
     * 查询Lambda表达式中被调用方法信息
     *
     * @param methodCallId
     * @return
     */
    public WriteDbData4LambdaMethodInfo getLambdaCalleeInfo(int methodCallId) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.LMI_QUERY_CALLEE_INFO;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.joinColumns(DC.LMI_LAMBDA_CALLEE_CLASS_NAME, DC.LMI_LAMBDA_CALLEE_METHOD_NAME) +
                    " from " + DbTableInfoEnum.DTIE_LAMBDA_METHOD_INFO.getTableName() +
                    " where " + DC.LMI_CALL_ID + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryObject(sql, WriteDbData4LambdaMethodInfo.class, methodCallId);
    }
}
