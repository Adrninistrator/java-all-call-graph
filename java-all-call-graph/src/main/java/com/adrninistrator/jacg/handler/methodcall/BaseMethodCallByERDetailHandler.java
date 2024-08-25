package com.adrninistrator.jacg.handler.methodcall;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.method.MethodDetail;
import com.adrninistrator.jacg.dto.methodcall.ObjArgsInfoInMethodCall;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCall;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.handler.classes.ClassInfoHandler;
import com.adrninistrator.jacg.handler.querybypage.QueryByPageHandler;
import com.adrninistrator.jacg.handler.querybypage.callback.QueryByPageCallBack;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/7/4
 * @description: 查询指定的调用方法对应的方法调用并处理，包含方法调用中被调用对象与参数的详情
 */
public abstract class BaseMethodCallByERDetailHandler extends BaseHandler implements QueryByPageCallBack<WriteDbData4MethodCall> {
    private static final Logger logger = LoggerFactory.getLogger(BaseMethodCallByERDetailHandler.class);

    protected ClassInfoHandler classInfoHandler;
    protected MethodCallInfoHandler methodCallInfoHandler;

    public BaseMethodCallByERDetailHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
        // 初始化
        init();
    }

    public BaseMethodCallByERDetailHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
        // 初始化
        init();
    }

    /**
     * 处理方法调用及对应的调用对象与参数信息
     *
     * @param methodCall              方法调用
     * @param callerMethodDetail      调用方法详细信息，包含了方法名称、方法参数等
     * @param calleeMethodDetail      被调用方法详细信息，包含了方法名称、方法参数等
     * @param objArgsInfoInMethodCall 方法调用中被调用对象与参数使用的信息
     */
    protected abstract void handleMethodWithArgs(WriteDbData4MethodCall methodCall, MethodDetail callerMethodDetail, MethodDetail calleeMethodDetail,
                                                 ObjArgsInfoInMethodCall objArgsInfoInMethodCall);

    @Override
    public int queryCurrentEndId(int currentStartId, Object... argsByPage) {
        String callerMethodHash = JACGUtil.getArgAt(0, argsByPage);
        return queryEndCallIdERMHByPage(callerMethodHash, currentStartId);
    }

    @Override
    public List<WriteDbData4MethodCall> queryDataByPage(int currentStartId, int currentEndId, boolean lastQuery, Object... argsByPage) {
        String callerMethodHash = JACGUtil.getArgAt(0, argsByPage);
        List<String> calleeSimpleClassNameList = JACGUtil.getArgAt(1, argsByPage);
        return queryMethodCallByPage(callerMethodHash, calleeSimpleClassNameList, lastQuery, currentStartId, currentEndId);
    }

    @Override
    public boolean handleDataList(List<WriteDbData4MethodCall> dataList, Object... argsByPage) throws Exception {
        for (WriteDbData4MethodCall methodCall : dataList) {
            MethodDetail callerMethodDetail = JACGClassMethodUtil.genMethodDetail(methodCall.getCallerFullMethod());
            MethodDetail calleeMethodDetail = JACGClassMethodUtil.genMethodDetail(methodCall.getCalleeFullMethod());
            // 查询方法调用中被调用对象与参数使用的信息
            ObjArgsInfoInMethodCall objArgsInfoInMethodCall = methodCallInfoHandler.queryObjArgsInfoInMethodCall(methodCall.getCallId());
            // 处理方法调用及对应的调用对象与参数信息
            handleMethodWithArgs(methodCall, callerMethodDetail, calleeMethodDetail, objArgsInfoInMethodCall);
        }
        return true;
    }

    // 初始化
    protected void init() {
        classInfoHandler = new ClassInfoHandler(dbOperWrapper);
        methodCallInfoHandler = new MethodCallInfoHandler(dbOperWrapper);
    }

    /**
     * 分页查询方法调用表，找到指定的调用方法并处理
     *
     * @param callerFullMethod    指定的调用方法的完整方法
     * @param calleeClassNameList 指定的被调用类名列表
     * @return true: 处理成功 false: 处理失败
     */
    public boolean handleMethodCallByER(String callerFullMethod, List<String> calleeClassNameList) {
        String callerMethodHash = JACGUtil.genHashWithLen(callerFullMethod);

        // 获得被调用类名的简单类名列表
        List<String> calleeSimpleClassNameList = new ArrayList<>(calleeClassNameList.size());
        for (String calleeClassName : calleeClassNameList) {
            String simpleCalleeClassName = classInfoHandler.queryExactlySimpleClassName(calleeClassName);
            if (simpleCalleeClassName != null) {
                calleeSimpleClassNameList.add(simpleCalleeClassName);
            }
        }
        if (calleeSimpleClassNameList.isEmpty()) {
            logger.error("指定的被调用类不存在调用关系\n{}", StringUtils.join(calleeClassNameList, "\n"));
            return true;
        }

        // 从方法调用表查询指定调用方法最小的call_id
        Integer minCallId = queryMinCallId(callerMethodHash);
        if (minCallId == null) {
            logger.error("从方法调用表未查询到指定的调用方法 {} {}", callerFullMethod, callerMethodHash);
            return true;
        }

        // 分页查询并处理
        return QueryByPageHandler.queryAndHandle(this, minCallId - 1, callerMethodHash, calleeSimpleClassNameList);
    }

    // 从方法调用表查询指定调用方法最小的call_id
    private Integer queryMinCallId(String callerMethodHash) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MC_QUERY_MIN_CALL_ID_CALLER_METHOD;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select min(" + DC.MC_CALL_ID + ")" +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName() +
                    " where " + DC.MC_CALLER_METHOD_HASH + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryObjectOneColumn(sql, Integer.class, callerMethodHash);
    }

    // 分页查询方法调用列表
    private List<WriteDbData4MethodCall> queryMethodCallByPage(String callerMethodHash, List<String> calleeSimpleClassNameList, boolean lastQuery, int startCallId, int endCallId) {
        SqlKeyEnum sqlKeyEnum = lastQuery ? SqlKeyEnum.MC_QUERY_ALL_BY_CALLER_METHOD_LAST : SqlKeyEnum.MC_QUERY_ALL_BY_CALLER_METHOD;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum, calleeSimpleClassNameList.size());
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_METHOD_CALL) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName() +
                    " where " + DC.MC_CALLER_METHOD_HASH + " = ?" +
                    " and " + DC.MC_CALLEE_SIMPLE_CLASS_NAME + " in " + JACGSqlUtil.genQuestionString(calleeSimpleClassNameList.size()) +
                    " and " + DC.MC_CALL_ID + " > ?";
            if (!lastQuery) {
                sql = sql + " and " + DC.MCI_CALL_ID + " <= ?";
            }
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql, calleeSimpleClassNameList.size());
        }
        List<Object> argList = new ArrayList<>();
        argList.add(callerMethodHash);
        argList.addAll(calleeSimpleClassNameList);
        argList.add(startCallId);
        if (!lastQuery) {
            argList.add(endCallId);
        }
        return dbOperator.queryList(sql, WriteDbData4MethodCall.class, argList.toArray());
    }
}
