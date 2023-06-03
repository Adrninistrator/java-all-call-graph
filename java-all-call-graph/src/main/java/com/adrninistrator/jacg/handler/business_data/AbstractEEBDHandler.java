package com.adrninistrator.jacg.handler.business_data;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.DbInsertMode;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.MethodCallFlagsEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.method.ClassAndMethodName;
import com.adrninistrator.jacg.dto.method_call.ObjArgsInfoInMethodCall;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.handler.method.MethodCallHandler;
import com.adrninistrator.jacg.handler.method.MethodCallInfoHandler;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.exceptions.JavaCGRuntimeException;
import org.apache.commons.lang3.ArrayUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/4/7
 * @description: 根据被调用方法处理业务功能数据的抽象父类，callee business data
 */
public abstract class AbstractEEBDHandler extends BaseHandler {
    private static final Logger logger = LoggerFactory.getLogger(AbstractEEBDHandler.class);

    // 记录需要处理的被调用方法列表
    private final List<ClassAndMethodName> calleeMethodList = new ArrayList<>();

    private MethodCallHandler methodCallHandler;

    private MethodCallInfoHandler methodCallInfoHandler;

    // 保存当前处理的被调用方法参数类型列表
    private List<String> currentCalleeMethodArgTypeList;

    public AbstractEEBDHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
        // 初始化
        init();
    }

    public AbstractEEBDHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
        // 初始化
        init();
    }

    /**
     * 选择当前类需要处理的被调用方法信息
     * 格式为：[类名]:[方法名]
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
     * @param methodCallId                   方法调用ID
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
            throw new JavaCGRuntimeException("未指定需要处理的被调用方法信息");
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

    // 分页查询本次从方法调用表查询的最大的call_id
    private int queryMaxCallIdByPage(String calleeSimpleClassName, int startCallId) {
        logger.debug("分页查询startCallId {} {}", calleeSimpleClassName, startCallId);
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MC_QUERY_BY_PAGE_MAX_CALL_ID;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + DC.MC_CALL_ID +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName() +
                    " where " + DC.MC_CALLEE_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.MC_CALL_ID + " > ?" +
                    " limit ?, 1";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        Integer endCallId = dbOperator.queryObjectOneColumn(sql, Integer.class, calleeSimpleClassName, startCallId, JACGConstants.DB_PAGE_HANDLE_SIZE - 1);
        if (endCallId == null) {
            // 最后一次分页查询
            logger.debug("最后一次分页查询 {}", startCallId);
            return JACGConstants.PAGE_QUERY_LAST;
        }

        // 不是最后一次分页查询
        logger.debug("查询到endCallId {} {}", startCallId, endCallId);
        return endCallId;
    }

    // 分页查询方法调用ID列表
    private List<Integer> queryMethodCallIdByPage(boolean lastQuery, String calleeSimpleClassName, String calleeMethodName, int startCallId, int endCallId) {
        SqlKeyEnum sqlKeyEnum = lastQuery ? SqlKeyEnum.MC_QUERY_CALL_ID_BY_CALLEE_LAST : SqlKeyEnum.MC_QUERY_CALL_ID_BY_CALLEE;
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
        String calleeSimpleClassName = dbOperWrapper.getSimpleClassName(calleeClassName);
        // 第一次分页查询时，从call_id最小值开始查询
        int startCallId = JavaCGConstants.METHOD_CALL_ID_START;
        int handleTime = 0;
        while (true) {
            // 分页查询本次查询的最大的call_id
            int endCallId = queryMaxCallIdByPage(calleeSimpleClassName, startCallId);
            if (endCallId == JACGConstants.PAGE_QUERY_FAIL) {
                // 查询失败
                return false;
            }

            // 判断是否为最后一次分页查询
            boolean lastQuery = (endCallId == JACGConstants.PAGE_QUERY_LAST);
            // 分页查询方法调用ID列表
            List<Integer> list = queryMethodCallIdByPage(lastQuery, calleeSimpleClassName, calleeMethodName, startCallId, endCallId);
            if (list == null) {
                // 查询失败
                return false;
            }

            if (!list.isEmpty()) {
                for (Integer methodCallId : list) {
                    if (handleTime == 0) {
                        // 第一次处理时，查询当前处理的被调用方法参数类型列表
                        String calleeFullMethod = dbOperWrapper.getCalleeFullMethodById(methodCallId);
                        if (calleeFullMethod == null) {
                            // 查询失败
                            return false;
                        }
                        currentCalleeMethodArgTypeList = JACGClassMethodUtil.genMethodArgTypeList(calleeFullMethod);
                    }

                    // 查询方法调用中被调用对象与参数使用的信息
                    ObjArgsInfoInMethodCall objArgsInfoInMethodCall = methodCallInfoHandler.queryObjArgsInfoInMethodCall(methodCallId);
                    // 处理方法调用
                    String businessData = handleMethodCall(methodCallId, calleeClassName, calleeMethodName, objArgsInfoInMethodCall, currentCalleeMethodArgTypeList);
                    if (businessData != null) {
                        /*
                            返回数据非空时进行处理：
                            向业务功能数据表写入数据
                            更新方法调用表对应记录的方法调用标志，设置被调用方法存在自定义的业务功能数据
                         */
                        if (!dbOperator.insert(insertSql, methodCallId, chooseBusinessDataType(), businessData)
                                || !methodCallHandler.updateMethodCallAddFlags(methodCallId, MethodCallFlagsEnum.MCFE_EE_BUSINESS_DATA)) {
                            return false;
                        }
                    }
                    handleTime++;
                }
            }

            if (lastQuery) {
                // 最后一次分页查询
                break;
            }
            startCallId = endCallId + 1;
        }
        return true;
    }
}
