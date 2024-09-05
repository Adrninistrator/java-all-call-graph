package com.adrninistrator.jacg.handler.methodcallargs;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.method.MethodDetail;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCall;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCallInfo;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.handler.methodcall.MethodCallHandler;
import com.adrninistrator.jacg.handler.querybypage.QueryByPageHandler;
import com.adrninistrator.jacg.handler.querybypage.callback.QueryByPageCallBack;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGMethodCallInfoUtil;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.common.enums.JavaCG2MethodCallInfoTypeEnum;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;
import org.apache.commons.lang3.ArrayUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/6/28
 * @description: 根据方法调用中使用的参数信息，获取对应的方法调用信息并处理
 */
public abstract class BaseMethodCallByArgsHandler extends BaseHandler implements QueryByPageCallBack<WriteDbData4MethodCallInfo> {
    private static final Logger logger = LoggerFactory.getLogger(BaseMethodCallByArgsHandler.class);

    protected MethodCallHandler methodCallHandler;

    protected List<String> methodCallInfoTypeList;

    public BaseMethodCallByArgsHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
        // 初始化
        init();
    }

    public BaseMethodCallByArgsHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
        // 初始化
        init();
    }

    /**
     * 选择需要查询的方法调用信息表中的类型
     *
     * @return
     */
    protected abstract JavaCG2MethodCallInfoTypeEnum[] chooseMethodCallInfoTypes();

    /**
     * 判断是否需要处理当前查询到的方法调用参数信息
     *
     * @param methodCallInfo
     * @return true: 需要处理 false: 不需要处理
     */
    protected abstract boolean needHandleMethodCallInfo(WriteDbData4MethodCallInfo methodCallInfo);

    /**
     * 处理方法调用及对应的方法调用参数信息
     *
     * @param methodCall         方法调用
     * @param callerMethodDetail 调用方法详细信息，包含了方法名称、方法参数等
     * @param calleeMethodDetail 被调用方法详细信息，包含了方法名称、方法参数等
     * @param methodCallInfo     方法调用信息
     */
    protected abstract void handleMethodCallWithInfo(WriteDbData4MethodCall methodCall, MethodDetail callerMethodDetail, MethodDetail calleeMethodDetail,
                                                     WriteDbData4MethodCallInfo methodCallInfo);

    @Override
    public int queryCurrentEndId(int currentStartId, Object... argsByPage) {
        return queryEndIdByPage(currentStartId, DbTableInfoEnum.DTIE_METHOD_CALL_INFO, DC.MCI_RECORD_ID);
    }

    @Override
    public List<WriteDbData4MethodCallInfo> queryDataByPage(int currentStartId, int currentEndId, boolean lastQuery, Object... argsByPage) {
        SqlKeyEnum sqlKeyEnum = lastQuery ? SqlKeyEnum.MCI_QUERY_ALL_BY_ID_TYPE_ARG_LAST : SqlKeyEnum.MCI_QUERY_ALL_BY_ID_TYPE_ARG;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum, methodCallInfoTypeList.size());
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_METHOD_CALL_INFO) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL_INFO.getTableName() +
                    " where " + DC.MCI_OBJ_ARGS_SEQ + " > ?" +
                    " and " + DC.MCI_TYPE + " in " + JACGSqlUtil.genQuestionString(methodCallInfoTypeList.size()) +
                    " and " + DC.MCI_RECORD_ID + " > ?";
            if (!lastQuery) {
                sql = sql + " and " + DC.MCI_RECORD_ID + " <= ?";
            }
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql, methodCallInfoTypeList.size());
        }
        List<Object> argList = new ArrayList<>();
        argList.add(JavaCG2Constants.METHOD_CALL_OBJECT_SEQ);
        argList.addAll(methodCallInfoTypeList);
        argList.add(currentStartId);
        if (!lastQuery) {
            argList.add(currentEndId);
        }
        List<WriteDbData4MethodCallInfo> list = dbOperator.queryList(sql, WriteDbData4MethodCallInfo.class, argList.toArray());
        if (list == null) {
            return null;
        }

        for (WriteDbData4MethodCallInfo writeDbData4MethodCallInfo : list) {
            // 对方法调用信息的值进行转换
            JACGMethodCallInfoUtil.transferValue(writeDbData4MethodCallInfo);
        }
        return list;
    }

    @Override
    public boolean handleDataList(List<WriteDbData4MethodCallInfo> dataList, Object... argsByPage) throws Exception {
        for (WriteDbData4MethodCallInfo methodCallInfo : dataList) {
            // 判断是否需要处理当前查询到的方法调用参数信息
            if (needHandleMethodCallInfo(methodCallInfo)) {
                WriteDbData4MethodCall methodCall = methodCallHandler.queryMethodCallByCallId(methodCallInfo.getCallId());
                if (methodCall == null) {
                    logger.warn("未查询到指定的方法调用，可能是同一个方法的递归调用未写入数据库 {}", methodCallInfo.getCallId());
                    continue;
                }
                MethodDetail callerMethodDetail = JACGClassMethodUtil.genMethodDetail(methodCall.getCallerFullMethod());
                MethodDetail calleeMethodDetail = JACGClassMethodUtil.genMethodDetail(methodCall.getCalleeFullMethod());
                // 处理方法调用及对应的方法调用参数信息
                handleMethodCallWithInfo(methodCall, callerMethodDetail, calleeMethodDetail, methodCallInfo);
            }
        }
        return true;
    }

    // 初始化
    protected void init() {
        JavaCG2MethodCallInfoTypeEnum[] methodCallInfoTypes = chooseMethodCallInfoTypes();
        if (ArrayUtils.isEmpty(methodCallInfoTypes)) {
            throw new JavaCG2RuntimeException("选择需要查询的方法调用信息表中的类型不允许为空");
        }

        methodCallHandler = new MethodCallHandler(dbOperWrapper);
        methodCallInfoTypeList = new ArrayList<>(methodCallInfoTypes.length);
        for (JavaCG2MethodCallInfoTypeEnum javaCG2MethodCallInfoTypeEnum : methodCallInfoTypes) {
            methodCallInfoTypeList.add(javaCG2MethodCallInfoTypeEnum.getType());
        }
    }

    /**
     * 分页查询方法调用信息表，找到指定的参数并处理
     *
     * @return true: 处理成功 false: 处理失败
     */
    public boolean handleMethodCallByArgs() {
        // 分页查询并处理
        return QueryByPageHandler.queryAndHandle(this, JavaCG2Constants.RECORD_ID_MIN_BEFORE);
    }
}
