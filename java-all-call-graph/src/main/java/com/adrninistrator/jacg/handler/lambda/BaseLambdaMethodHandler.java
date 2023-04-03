package com.adrninistrator.jacg.handler.lambda;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dto.lambda.LambdaMethodCall;
import com.adrninistrator.jacg.dto.lambda.LambdaMethodCallDetail;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.common.enums.JavaCGYesNoEnum;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * @author adrninistrator
 * @date 2023/1/10
 * @description: Lambda表达式方法相关信息查询处理基类
 */
public abstract class BaseLambdaMethodHandler extends BaseHandler {
    private static final Logger logger = LoggerFactory.getLogger(BaseLambdaMethodHandler.class);

    protected BaseLambdaMethodHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
    }

    // 以下分别为查询非最后一次、查询最后一次的sql语句，会进行缓存，因此当前类的子类在针对不同app.name的配置使用时，需要创建新的实例
    protected String cachedQuerySqlNotLast = null;
    protected String cachedQuerySqlLast = null;

    protected AtomicBoolean runningFlag = new AtomicBoolean(false);

    /**
     * 分页查询
     *
     * @param startCallId 分页查询call_id起始id（大于）
     * @param endCallId   分页查询call_id结束id（小于等于）
     * @param args        其他参数
     * @return
     */
    protected abstract List<Map<String, Object>> queryByPage(int startCallId, int endCallId, Object... args);

    /**
     * 执行查询操作
     *
     * @return
     */
    public List<LambdaMethodCall> query(Object... args) {
        if (!runningFlag.compareAndSet(false, true)) {
            logger.error("当前类不允许并发调用，请创建新的实例");
            return null;
        }

        try {
            List<LambdaMethodCall> lambdaMethodCallList = new ArrayList<>(100);

            // 第一次分页查询时，从call_id最小值开始查询
            int startCallId = JavaCGConstants.METHOD_CALL_ID_START;
            while (true) {
                // 分页查询本次从Lambda表达式方法信息表查询的最大的call_id
                int endCallId = queryMaxCallIdByPage(startCallId);
                if (endCallId == JACGConstants.PAGE_QUERY_FAIL) {
                    // 查询失败
                    return null;
                }

                // 通过类名前缀分页查询Lambda表达式方法调用信息
                List<Map<String, Object>> list = queryByPage(startCallId, endCallId, args);
                if (list == null) {
                    return null;
                }

                // 添加查询结果列表
                addLambdaMethodCall(lambdaMethodCallList, list);

                if (endCallId == JACGConstants.PAGE_QUERY_LAST) {
                    // 最后一次分页查询
                    break;
                }
                startCallId = endCallId + 1;
            }
            return lambdaMethodCallList;
        } finally {
            runningFlag.set(false);
        }
    }

    // 分页查询本次从Lambda表达式方法信息表查询的最大的call_id
    private int queryMaxCallIdByPage(int startCallId) {
        logger.debug("分页查询startCallId {}", startCallId);
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.LMI_QUERY_MAX_CALL_ID;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + DC.LMI_CALL_ID +
                    " from " + DbTableInfoEnum.DTIE_LAMBDA_METHOD_INFO.getTableName() +
                    " where " + DC.LMI_CALL_ID + " > ?" +
                    " limit ?, 1";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        List<Object> list = dbOperator.queryListOneColumn(sql, new Object[]{startCallId, JACGConstants.DB_QUERY_PAGE_SIZE});
        if (list == null) {
            // 查询失败
            logger.debug("查询失败 {}", startCallId);
            return JACGConstants.PAGE_QUERY_FAIL;
        }

        if (list.isEmpty()) {
            // 最后一次分页查询
            logger.debug("最后一次分页查询 {}", startCallId);
            return JACGConstants.PAGE_QUERY_LAST;
        }

        // 不是最后一次分页查询
        int endCallId = (int) list.get(0);
        logger.debug("查询到endCallId {}", endCallId);
        return endCallId;
    }

    // 生成查询使用的sql语句，分页查询最后一页与非最后一页时分别处理
    protected String genQuerySql(boolean lastQuery) {
        if (cachedQuerySqlLast == null) {
            cachedQuerySqlLast = "select " + JACGSqlUtil.joinColumns(
                    "mc." + DC.MC_CALL_ID,
                    "mc." + DC.MC_CALLER_FULL_METHOD,
                    "mc." + DC.MC_CALLER_LINE_NUMBER,
                    "mc." + DC.MC_CALLEE_FULL_METHOD,
                    "lmi." + DC.LMI_LAMBDA_CALLEE_FULL_METHOD,
                    "lmi." + DC.LMI_LAMBDA_NEXT_FULL_METHOD,
                    "lmi." + DC.LMI_LAMBDA_NEXT_IS_STREAM,
                    "lmi." + DC.LMI_LAMBDA_NEXT_IS_INTERMEDIATE,
                    "lmi." + DC.LMI_LAMBDA_NEXT_IS_TERMINAL
            ) + " from " + DbTableInfoEnum.DTIE_LAMBDA_METHOD_INFO.getTableName() + " as lmi join " +
                    DbTableInfoEnum.DTIE_METHOD_CALL.getTableName() + " as mc" +
                    " on lmi." + DC.LMI_CALL_ID + " = mc." + DC.MC_CALL_ID +
                    " where lmi." + DC.LMI_CALL_ID + " > ?";
        }
        if (cachedQuerySqlNotLast == null) {
            cachedQuerySqlNotLast = cachedQuerySqlLast + " and lmi." + DC.LMI_CALL_ID + " <= ?";
        }

        return lastQuery ? cachedQuerySqlLast : cachedQuerySqlNotLast;
    }

    // 添加查询结果列表
    private void addLambdaMethodCall(List<LambdaMethodCall> lambdaMethodCallList, List<Map<String, Object>> list) {
        if (list.isEmpty()) {
            return;
        }

        for (Map<String, Object> map : list) {
            int callId = (int) map.get(DC.MC_CALL_ID);
            String callerFullMethod = (String) map.get(DC.MC_CALLER_FULL_METHOD);
            int callerLineNum = (int) map.get(DC.MC_CALLER_LINE_NUMBER);
            String calleeFullMethod = (String) map.get(DC.MC_CALLEE_FULL_METHOD);
            String lambdaCalleeFullMethod = (String) map.get(DC.LMI_LAMBDA_CALLEE_FULL_METHOD);

            LambdaMethodCall lambdaMethodCall = new LambdaMethodCall();
            lambdaMethodCallList.add(lambdaMethodCall);
            lambdaMethodCall.setCallId(callId);
            lambdaMethodCall.setCallerFullMethod(callerFullMethod);
            lambdaMethodCall.setCallerLineNumber(callerLineNum);
            lambdaMethodCall.setCalleeFullMethod(calleeFullMethod);
            lambdaMethodCall.setLambdaCalleeFullMethod(lambdaCalleeFullMethod);

            Object lambdaNextCalleeFullMethodObj = map.get(DC.LMI_LAMBDA_NEXT_FULL_METHOD);
            if (lambdaNextCalleeFullMethodObj == null) {
                continue;
            }

            String lambdaNextCalleeFullMethod = (String) lambdaNextCalleeFullMethodObj;
            boolean lambdaNextIsStream = JavaCGYesNoEnum.isYes((int) map.get(DC.LMI_LAMBDA_NEXT_IS_STREAM));
            boolean lambdaNextIsIntermediate = JavaCGYesNoEnum.isYes((int) map.get(DC.LMI_LAMBDA_NEXT_IS_INTERMEDIATE));
            boolean lambdaNextIsTerminal = JavaCGYesNoEnum.isYes((int) map.get(DC.LMI_LAMBDA_NEXT_IS_TERMINAL));

            lambdaMethodCall.setLambdaNextCalleeFullMethod(lambdaNextCalleeFullMethod);
            lambdaMethodCall.setLambdaNextIsStream(lambdaNextIsStream);
            lambdaMethodCall.setLambdaNextIsIntermediate(lambdaNextIsIntermediate);
            lambdaMethodCall.setLambdaNextIsTerminal(lambdaNextIsTerminal);
        }
    }

    /**
     * 转换Lambda表达式方法调用列表，获得Lambda表达式方法调用包含各方法的详细信息列表
     *
     * @param lambdaMethodCallList
     * @return
     */
    protected List<LambdaMethodCallDetail> genDetailList(List<LambdaMethodCall> lambdaMethodCallList) {
        if (lambdaMethodCallList == null) {
            return null;
        }

        List<LambdaMethodCallDetail> lambdaMethodCallDetailList = new ArrayList<>(lambdaMethodCallList.size());
        for (LambdaMethodCall lambdaMethodCall : lambdaMethodCallList) {
            LambdaMethodCallDetail lambdaMethodCallDetail = LambdaMethodCallDetail.genLambdaMethodCallDetail(lambdaMethodCall);
            lambdaMethodCallDetailList.add(lambdaMethodCallDetail);

            if (lambdaMethodCall.getCallerFullMethod() != null) {
                lambdaMethodCallDetail.setCallerFullMethodDetail(JACGClassMethodUtil.genMethodDetail(lambdaMethodCall.getCallerFullMethod()));
            }
            if (lambdaMethodCall.getCalleeFullMethod() != null) {
                lambdaMethodCallDetail.setCalleeFullMethodDetail(JACGClassMethodUtil.genMethodDetail(lambdaMethodCall.getCalleeFullMethod()));
            }
            if (lambdaMethodCall.getLambdaCalleeFullMethod() != null) {
                lambdaMethodCallDetail.setLambdaCalleeFullMethodDetail(JACGClassMethodUtil.genMethodDetail(lambdaMethodCall.getLambdaCalleeFullMethod()));
            }
            if (lambdaMethodCall.getLambdaNextCalleeFullMethod() != null) {
                lambdaMethodCallDetail.setLambdaNextCalleeFullMethodDetail(JACGClassMethodUtil.genMethodDetail(lambdaMethodCall.getLambdaNextCalleeFullMethod()));
            }
        }
        return lambdaMethodCallDetailList;
    }
}
