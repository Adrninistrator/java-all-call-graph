package com.adrninistrator.jacg.handler.lambda;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dto.lambda.LambdaMethodCall;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.handler.querybypage.callback.QueryByPageCallBack;
import com.adrninistrator.jacg.util.JACGSqlUtil;

/**
 * @author adrninistrator
 * @date 2023/1/10
 * @description: Lambda表达式方法相关信息分页查询处理基类
 */
public abstract class BaseLambdaMethodPageHandler extends BaseHandler implements QueryByPageCallBack<LambdaMethodCall> {

    protected BaseLambdaMethodPageHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
    }

    // 以下分别为查询非最后一次、查询最后一次的sql语句，会进行缓存，因此当前类的子类在针对不同app.name的配置使用时，需要创建新的实例
    protected String cachedQuerySqlNotLast = null;
    protected String cachedQuerySqlLast = null;

    @Override
    public int queryCurrentEndId(int currentStartId, Object... argsByPage) {
        return queryEndIdByPage(currentStartId, DbTableInfoEnum.DTIE_LAMBDA_METHOD_INFO, DC.LMI_CALL_ID);
    }

    // 生成查询使用的sql语句，分页查询最后一页与非最后一页时分别处理
    protected String genQuerySql(boolean lastQuery) {
        if (cachedQuerySqlLast == null) {
            cachedQuerySqlLast = "select " + JACGSqlUtil.joinColumns(
                    "mc." + DC.MC_CALL_ID,
                    "mc." + DC.MC_CALLER_FULL_METHOD,
                    "mc." + DC.MC_CALLER_LINE_NUMBER,
                    "mc." + DC.MC_CALLEE_FULL_METHOD,
                    "mc." + DC.MC_RAW_RETURN_TYPE,
                    "lmi." + DC.LMI_LAMBDA_CALLEE_FULL_METHOD,
                    "lmi." + DC.LMI_LAMBDA_NEXT_FULL_METHOD,
                    "lmi." + DC.LMI_LAMBDA_NEXT_IS_STREAM,
                    "lmi." + DC.LMI_LAMBDA_NEXT_IS_INTERMEDIATE,
                    "lmi." + DC.LMI_LAMBDA_NEXT_IS_TERMINAL
            ) + " from " + DbTableInfoEnum.DTIE_LAMBDA_METHOD_INFO.getTableName() + " as lmi join " +
                    DbTableInfoEnum.DTIE_METHOD_CALL.getTableName() + " as mc" +
                    " on lmi." + DC.LMI_CALL_ID + " = mc." + DC.MC_CALL_ID +
                    " where lmi." + DC.LMI_CALL_ID + " > ?";
            cachedQuerySqlLast = dbOperWrapper.formatSql(cachedQuerySqlLast);
        }
        if (cachedQuerySqlNotLast == null) {
            cachedQuerySqlNotLast = cachedQuerySqlLast + " and lmi." + DC.LMI_CALL_ID + " <= ?";
        }

        return lastQuery ? cachedQuerySqlLast : cachedQuerySqlNotLast;
    }
}
