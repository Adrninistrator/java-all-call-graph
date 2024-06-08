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
import com.adrninistrator.javacg.util.JavaCGUtil;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/7/2
 * @description: 查询指定的被调用类及方法对应的方法调用并处理，包含方法调用中被调用对象与参数的详情
 */
public abstract class BaseMethodCallByEEDetailHandler extends BaseHandler implements QueryByPageCallBack<WriteDbData4MethodCall> {
    private static final Logger logger = LoggerFactory.getLogger(BaseMethodCallByEEDetailHandler.class);

    protected final ClassInfoHandler classInfoHandler;
    protected final MethodCallHandler methodCallHandler;
    protected final MethodCallInfoHandler methodCallInfoHandler;

    public BaseMethodCallByEEDetailHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
        classInfoHandler = new ClassInfoHandler(dbOperWrapper);
        methodCallHandler = new MethodCallHandler(dbOperWrapper);
        methodCallInfoHandler = new MethodCallInfoHandler(dbOperWrapper);
    }

    public BaseMethodCallByEEDetailHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
        classInfoHandler = new ClassInfoHandler(dbOperWrapper);
        methodCallHandler = new MethodCallHandler(dbOperWrapper);
        methodCallInfoHandler = new MethodCallInfoHandler(dbOperWrapper);
    }

    /**
     * 处理方法调用及对应的调用对象与参数信息
     *
     * @param methodCall              方法调用
     * @param callerMethodDetail      调用方法详细信息，包含了方法名称、方法参数等
     * @param calleeMethodDetail      被调用方法详细信息，包含了方法名称、方法参数等
     * @param objArgsInfoInMethodCall 方法调用中被调用对象与参数使用的信息
     * @param args                    传递其他参数，若不需要使用则不指定
     */
    protected abstract void handleMethodWithArgs(WriteDbData4MethodCall methodCall, MethodDetail callerMethodDetail, MethodDetail calleeMethodDetail,
                                                 ObjArgsInfoInMethodCall objArgsInfoInMethodCall, Object... args);

    @Override
    public int queryCurrentEndId(int currentStartId, Object... argsByPage) {
        String calleeSimpleClassName = JACGUtil.getArgAt(1, argsByPage);
        return queryEndCallIdEESCNByPage(calleeSimpleClassName, currentStartId);
    }

    @Override
    public List<WriteDbData4MethodCall> queryDataByPage(int currentStartId, int currentEndId, boolean lastQuery, Object... argsByPage) {
        String calleeSimpleClassName = JACGUtil.getArgAt(1, argsByPage);
        List<String> methodNameList = JACGUtil.getArgAt(2, argsByPage);
        return queryMethodCallByPage(calleeSimpleClassName, methodNameList, lastQuery, currentStartId, currentEndId);
    }

    @Override
    public boolean handleDataList(List<WriteDbData4MethodCall> dataList, Object... argsByPage) throws Exception {
        Object[] args = JACGUtil.getArgAt(0, argsByPage);
        for (WriteDbData4MethodCall methodCall : dataList) {
            MethodDetail callerMethodDetail = JACGClassMethodUtil.genMethodDetail(methodCall.getCallerFullMethod());
            MethodDetail calleeMethodDetail = JACGClassMethodUtil.genMethodDetail(methodCall.getCalleeFullMethod());
            // 选择是否需要处理当前方法调用
            if (!chooseHandleMethod(methodCall, callerMethodDetail, calleeMethodDetail)) {
                continue;
            }

            ObjArgsInfoInMethodCall objArgsInfoInMethodCall = null;
            // 选择是否需要查询方法调用中被调用对象与参数使用的信息
            if (chooseQueryObjArgsInfoInMethodCall()) {
                // 查询方法调用中被调用对象与参数使用的信息
                objArgsInfoInMethodCall = methodCallInfoHandler.queryObjArgsInfoInMethodCall(methodCall.getCallId());
            }
            // 处理方法调用及对应的调用对象与参数信息
            handleMethodWithArgs(methodCall, callerMethodDetail, calleeMethodDetail, objArgsInfoInMethodCall, args);
        }
        return true;
    }

    /**
     * 选择是否需要处理当前方法调用
     *
     * @param methodCall         方法调用
     * @param callerMethodDetail 调用方法详细信息，包含了方法名称、方法参数等
     * @param calleeMethodDetail 被调用方法详细信息，包含了方法名称、方法参数等
     * @return true: 需要处理 false: 不需要处理
     */
    protected boolean chooseHandleMethod(WriteDbData4MethodCall methodCall, MethodDetail callerMethodDetail, MethodDetail calleeMethodDetail) {
        // 默认都需要处理
        return true;
    }

    /**
     * 选择是否需要查询方法调用中被调用对象与参数使用的信息
     *
     * @return true: 需要查询 false: 不需要查询
     */
    protected boolean chooseQueryObjArgsInfoInMethodCall() {
        // 默认都需要处理
        return true;
    }

    /**
     * 分页查询方法调用表，找到指定被调用类且名称满足要求的方法的调用情况并处理
     *
     * @param calleeClassName   指定的被调用类名，需要是完整类名
     * @param methodNameKeyword 指定的被调用方法名关键字
     * @param args              传递其他参数，若不需要使用则不指定
     * @return true: 处理成功 false: 处理失败
     */
    public boolean handleMethodCallByEECMKeyword(String calleeClassName, String methodNameKeyword, Object... args) {
        List<String> methodNameList = methodCallHandler.getCalleeMethodNameLike(calleeClassName, methodNameKeyword);
        if (JavaCGUtil.isCollectionEmpty(methodNameList)) {
            logger.error("根据方法关键字查询到被调用方法为空 {} {}", calleeClassName, methodNameKeyword);
            return true;
        }
        logger.info("根据方法关键字查询到被调用方法 {} {}\n{}", calleeClassName, methodNameKeyword, StringUtils.join(methodNameList, "\n"));
        return handleMethodCallByEECM(calleeClassName, methodNameList, args);
    }

    /**
     * 分页查询方法调用表，找到指定被调用类且名称满足要求的方法的调用情况并处理
     *
     * @param calleeClassName       指定的被调用类名，需要是完整类名
     * @param methodNameKeywordList 指定的被调用方法名关键字列表
     * @param args                  传递其他参数，若不需要使用则不指定
     * @return true: 处理成功 false: 处理失败
     */
    public boolean handleMethodCallByEECMKeyword(String calleeClassName, List<String> methodNameKeywordList, Object... args) {
        List<String> methodNameList = new ArrayList<>();
        for (String methodNameKeyword : methodNameKeywordList) {
            List<String> tmpMethodNameList = methodCallHandler.getCalleeMethodNameLike(calleeClassName, methodNameKeyword);
            if (!JavaCGUtil.isCollectionEmpty(tmpMethodNameList)) {
                methodNameList.addAll(tmpMethodNameList);
            }
        }
        if (JavaCGUtil.isCollectionEmpty(methodNameList)) {
            logger.error("根据方法关键字查询到被调用方法为空 {}\n[{}]", calleeClassName, StringUtils.join(methodNameKeywordList, "\n"));
            return true;
        }
        logger.info("根据方法关键字查询到被调用方法 {}\n[{}]\n[{}]", calleeClassName, StringUtils.join(methodNameKeywordList, "\n"), StringUtils.join(methodNameList, "\n"));
        return handleMethodCallByEECM(calleeClassName, methodNameList, args);
    }

    /**
     * 分页查询方法调用表，找到指定被调用类的方法调用情况并处理
     *
     * @param calleeClassName 指定的被调用类名，需要是完整类名
     * @param args            传递其他参数，若不需要使用则不指定
     * @return true: 处理成功 false: 处理失败
     */
    public boolean handleMethodCallByEEC(String calleeClassName, Object... args) {
        return handleMethodCallByEECM(calleeClassName, null, args);
    }

    /**
     * 分页查询方法调用表，找到指定被调用类及方法的方法并处理
     *
     * @param calleeClassName 指定的被调用类名，需要是完整类名
     * @param methodNameList  指定的被调用方法名列表，若为空则查询指定类的所有方法调用情况
     * @param args            传递其他参数，若不需要使用则不指定
     * @return true: 处理成功 false: 处理失败
     */
    public boolean handleMethodCallByEECM(String calleeClassName, List<String> methodNameList, Object... args) {
        String calleeSimpleClassName = classInfoHandler.getExactlySimpleClassName(calleeClassName);
        if (calleeSimpleClassName == null) {
            return true;
        }

        // 从方法调用表查询指定被调用类最小的call_id
        Integer minCallId = methodCallHandler.queryMinCallIdEESCN(calleeSimpleClassName);
        if (minCallId == null) {
            logger.error("从方法调用表未查询到指定的被调用类 {}", calleeSimpleClassName);
            return true;
        }

        // 假如当前方法的自定义参数为空，则设置为空数组，在调用分页查询时指定到第1个自定义参数
        Object[] usedArgs = args;
        if (usedArgs == null) {
            usedArgs = new Object[]{};
        }
        // 分页查询并处理
        return QueryByPageHandler.queryAndHandle(this, minCallId - 1, usedArgs, calleeSimpleClassName, methodNameList);
    }

    // 分页查询类的指定方法调用列表
    private List<WriteDbData4MethodCall> queryMethodCallByPage(String calleeClassName, List<String> methodNameList, boolean lastQuery, int startCallId, int endCallId) {
        if (JavaCGUtil.isCollectionEmpty(methodNameList)) {
            // 分页查询类的所有方法调用列表
            return queryAllMethodsCallByPage(calleeClassName, lastQuery, startCallId, endCallId);
        }

        SqlKeyEnum sqlKeyEnum = lastQuery ? SqlKeyEnum.MC_QUERY_ALL_BY_CALLEE_CLASS_METHODS_LAST : SqlKeyEnum.MC_QUERY_ALL_BY_CALLEE_CLASS_METHODS;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum, methodNameList.size());
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_METHOD_CALL) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName() +
                    " where " + DC.MC_CALLEE_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.MC_CALLEE_METHOD_NAME + " in " + JACGSqlUtil.genQuestionString(methodNameList.size()) +
                    " and " + DC.MC_CALL_ID + " > ?";
            if (!lastQuery) {
                sql = sql + " and " + DC.MCI_CALL_ID + " <= ?";
            }
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql, methodNameList.size());
        }
        List<Object> argList = new ArrayList<>();
        argList.add(calleeClassName);
        argList.addAll(methodNameList);
        argList.add(startCallId);
        if (!lastQuery) {
            argList.add(endCallId);
        }
        return dbOperator.queryList(sql, WriteDbData4MethodCall.class, argList.toArray());
    }

    // 分页查询类的所有方法调用列表
    private List<WriteDbData4MethodCall> queryAllMethodsCallByPage(String calleeClassName, boolean lastQuery, int startCallId, int endCallId) {
        SqlKeyEnum sqlKeyEnum = lastQuery ? SqlKeyEnum.MC_QUERY_ALL_BY_CALLEE_CLASS_LAST : SqlKeyEnum.MC_QUERY_ALL_BY_CALLEE_CLASS;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_METHOD_CALL) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName() +
                    " where " + DC.MC_CALLEE_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.MC_CALL_ID + " > ?";
            if (!lastQuery) {
                sql = sql + " and " + DC.MCI_CALL_ID + " <= ?";
            }
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        List<Object> argList = new ArrayList<>();
        argList.add(calleeClassName);
        argList.add(startCallId);
        if (!lastQuery) {
            argList.add(endCallId);
        }
        return dbOperator.queryList(sql, WriteDbData4MethodCall.class, argList.toArray());
    }
}
