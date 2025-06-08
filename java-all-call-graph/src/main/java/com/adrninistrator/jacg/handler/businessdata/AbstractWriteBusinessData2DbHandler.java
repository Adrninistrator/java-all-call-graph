package com.adrninistrator.jacg.handler.businessdata;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.DbInsertMode;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.MethodCallFlagsEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.method.ClassAndMethodName;
import com.adrninistrator.jacg.dto.methodcall.ObjArgsInfoInMethodCall;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.handler.methodcall.MethodCallHandler;
import com.adrninistrator.jacg.handler.methodcall.MethodCallInfoHandler;
import com.adrninistrator.jacg.handler.querybypage.QueryByPageHandler;
import com.adrninistrator.jacg.handler.querybypage.callback.QueryByPageCallBack;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg2.dto.counter.JavaCG2Counter;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;
import org.apache.commons.lang3.ArrayUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/4/7
 * @description: 将业务功能数据写入数据库的抽象父类
 */
public abstract class AbstractWriteBusinessData2DbHandler extends BaseHandler implements QueryByPageCallBack<Integer> {
    private static final Logger logger = LoggerFactory.getLogger(AbstractWriteBusinessData2DbHandler.class);

    // 记录需要处理的被调用方法列表
    private final List<ClassAndMethodName> calleeMethodList = new ArrayList<>();

    private MethodCallHandler methodCallHandler;

    private MethodCallInfoHandler methodCallInfoHandler;

    public AbstractWriteBusinessData2DbHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
        // 初始化
        init();
    }

    public AbstractWriteBusinessData2DbHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
        // 初始化
        init();
    }

    /**
     * 选择当前类需要处理的被调用方法信息
     * 格式为：{类名}:{方法名}
     * 不需要指定方法参数
     *
     * @return
     */
    protected abstract String[] chooseCalleeMethodInfoArray();

    /**
     * 选择当前类需要处理的业务功能数据类型
     *
     * @return
     */
    public abstract String chooseBusinessDataType();

    /**
     * 执行处理方法调用
     *
     * @param methodCallId                   方法调用序号
     * @param calleeClassName                被调用类名
     * @param calleeMethodName               被调用方法名
     * @param objArgsInfoInMethodCall        方法调用中被调用对象与参数使用的信息，可能为null
     * @param currentCalleeMethodArgTypeList 当前处理的被调用方法参数类型列表，序号从0开始
     * @return 根据被调用的方法参数等信息生成的业务功能数据，字符串类型，可为JSON等格式，若非null则会写入业务功能数据表
     */
    protected abstract String handleMethodCall(int methodCallId,
                                               String calleeClassName,
                                               String calleeMethodName,
                                               ObjArgsInfoInMethodCall objArgsInfoInMethodCall,
                                               List<String> currentCalleeMethodArgTypeList);

    @Override
    public int queryCurrentEndId(int currentStartId, Object... argsByPage) {
        String calleeSimpleClassName = JACGUtil.getArgAt(0, argsByPage);
        return queryEndCallIdEESCNByPage(calleeSimpleClassName, currentStartId);
    }

    @Override
    public List<Integer> queryDataByPage(int currentStartId, int currentEndId, boolean lastQuery, Object... argsByPage) {
        String calleeSimpleClassName = JACGUtil.getArgAt(0, argsByPage);
        String calleeMethodName = JACGUtil.getArgAt(1, argsByPage);
        return queryMethodCallIdByPage(lastQuery, calleeSimpleClassName, calleeMethodName, currentStartId, currentEndId);
    }

    @Override
    public boolean handleDataList(List<Integer> dataList, Object... argsByPage) throws Exception {
        String calleeMethodName = JACGUtil.getArgAt(1, argsByPage);
        String calleeClassName = JACGUtil.getArgAt(2, argsByPage);
        String insertSql = JACGUtil.getArgAt(3, argsByPage);
        JavaCG2Counter handleTimes = JACGUtil.getArgAt(4, argsByPage);
        List<String> currentCalleeMethodArgTypeList = JACGUtil.getArgAt(5, argsByPage);
        for (Integer methodCallId : dataList) {
            if (handleTimes.getCount() == 0) {
                // 第一次处理时，查询当前处理的被调用方法参数类型列表
                String calleeFullMethod = methodCallHandler.queryCalleeFullMethodById(methodCallId);
                if (calleeFullMethod == null) {
                    // 查询失败
                    return false;
                }

                currentCalleeMethodArgTypeList.addAll(JACGClassMethodUtil.genMethodArgTypeList(calleeFullMethod));
            }

            // 查询方法调用中被调用对象与参数使用的信息
            ObjArgsInfoInMethodCall objArgsInfoInMethodCall = methodCallInfoHandler.queryObjArgsInfoInMethodCall(methodCallId);
            // 处理方法调用
            String businessData = handleMethodCall(methodCallId, calleeClassName, calleeMethodName, objArgsInfoInMethodCall, currentCalleeMethodArgTypeList);
            if (businessData != null) {
                /*
                    返回数据非空时进行处理：
                    向业务功能数据表写入数据
                    更新方法调用表对应记录的方法调用标志，设置被调用方法存在业务功能数据
                 */
                if (!dbOperator.insert(insertSql, methodCallId, chooseBusinessDataType(), businessData)
                        || !methodCallHandler.updateMethodCallAddFlags(methodCallId, MethodCallFlagsEnum.MCFE_EE_BUSINESS_DATA)) {
                    return false;
                }
            }
            handleTimes.addAndGet();
        }
        return true;
    }

    /**
     * 根据被调用方法处理对应的方法调用
     *
     * @return
     */
    public boolean handleMethodCallByCallee() {
        try {
            // 删除业务功能数据表中当前类型的数据
            SqlKeyEnum sqlKeyEnum = SqlKeyEnum.BD_DELETE_BY_TYPE;
            String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
            if (sql == null) {
                sql = "delete from " + DbTableInfoEnum.DTIE_BUSINESS_DATA.getTableName() +
                        " where " + DC.BD_DATA_TYPE + " = ?" +
                        " limit ?";
                sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
            }

            String businessDataType = chooseBusinessDataType();
            while (true) {
                Integer row = dbOperator.update(sql, businessDataType, JACGConstants.DB_PAGE_HANDLE_SIZE);
                if (row == null) {
                    return false;
                }
                logger.info("从业务功能数据表删除数据行数 {} {}", businessDataType, row);
                if (row < JACGConstants.DB_PAGE_HANDLE_SIZE) {
                    break;
                }
            }

            // 生成插入业务功能数据表的sql语句
            String insertSql = dbOperWrapper.genAndCacheInsertSql(DbTableInfoEnum.DTIE_BUSINESS_DATA, DbInsertMode.DIME_INSERT);
            for (ClassAndMethodName classAndMethodName : calleeMethodList) {
                // 根据指定的被调用方法查询方法调用
                if (!handleMethodCallByCalleeMethod(classAndMethodName.getClassName(), classAndMethodName.getMethodName(), insertSql)) {
                    return false;
                }
            }
            return true;
        } catch (Exception e) {
            logger.error("error ", e);
            return false;
        }
    }

    // 初始化
    protected void init() {
        // 处理指定的被调用方法信息
        String[] calleeMethodInfoArray = chooseCalleeMethodInfoArray();
        if (ArrayUtils.isEmpty(calleeMethodInfoArray)) {
            throw new JavaCG2RuntimeException("未指定需要处理的被调用方法信息");
        }

        for (String calleeMethodInfo : calleeMethodInfoArray) {
            ClassAndMethodName classAndMethodName = JACGClassMethodUtil.parseClassAndMethodName(calleeMethodInfo);
            if (!calleeMethodList.contains(classAndMethodName)) {
                calleeMethodList.add(classAndMethodName);
            }
        }

        methodCallHandler = new MethodCallHandler(dbOperWrapper);
        methodCallInfoHandler = new MethodCallInfoHandler(dbOperWrapper);
    }

    // 分页查询方法调用序号列表
    private List<Integer> queryMethodCallIdByPage(boolean lastQuery, String calleeSimpleClassName, String calleeMethodName, int startCallId, int endCallId) {
        SqlKeyEnum sqlKeyEnum = lastQuery ? SqlKeyEnum.MC_QUERY_CALL_ID_BY_CALLEE_CLASS_METHOD_LAST : SqlKeyEnum.MC_QUERY_CALL_ID_BY_CALLEE_CLASS_METHOD;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + DC.MC_CALL_ID +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName() +
                    " where " + DC.MC_CALLEE_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.MC_CALLEE_METHOD_NAME + " = ?" +
                    " and " + DC.MC_CALL_ID + " > ?";
            if (!lastQuery) {
                sql = sql + " and " + DC.MC_CALL_ID + " <= ?";
            }
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        List<Object> argList = new ArrayList<>();
        argList.add(calleeSimpleClassName);
        argList.add(calleeMethodName);
        argList.add(startCallId);
        if (!lastQuery) {
            argList.add(endCallId);
        }
        return dbOperator.queryListOneColumn(sql, Integer.class, argList.toArray());
    }

    /**
     * 根据指定的被调用方法查询方法调用并处理
     *
     * @param calleeClassName
     * @param calleeMethodName
     * @param insertSql
     * @return
     */
    private boolean handleMethodCallByCalleeMethod(String calleeClassName, String calleeMethodName, String insertSql) {
        String calleeSimpleClassName = dbOperWrapper.querySimpleClassName(calleeClassName);
        // 从方法调用表查询指定被调用类最小的call_id
        Integer minCallId = methodCallHandler.queryMinCallIdEESCN(calleeSimpleClassName);
        if (minCallId == null) {
            logger.error("从方法调用表未查询到指定的被调用类 {}", calleeSimpleClassName);
            return true;
        }

        // 保存当前处理的被调用方法参数类型列表
        List<String> currentCalleeMethodArgTypeList = new ArrayList<>();
        JavaCG2Counter handleTimes = new JavaCG2Counter(0);
        // 分页查询并处理
        return QueryByPageHandler.queryAndHandle(this, minCallId - 1, calleeSimpleClassName, calleeMethodName, calleeClassName, insertSql, handleTimes,
                currentCalleeMethodArgTypeList);
    }
}
