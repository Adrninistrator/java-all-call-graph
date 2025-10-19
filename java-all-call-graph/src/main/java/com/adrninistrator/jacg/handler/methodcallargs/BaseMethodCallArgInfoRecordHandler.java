package com.adrninistrator.jacg.handler.methodcallargs;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.conf.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.dto.method.MethodDetailNoReturnType;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCall;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.handler.conf.ConfigHandler;
import com.adrninistrator.jacg.handler.methodcall.MethodCallInfoHandler;
import com.adrninistrator.jacg.handler.querybypage.QueryByPageHandler;
import com.adrninistrator.jacg.handler.querybypage.callback.QueryByPageCallBack;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.jacg.writer.WriterSupportHeader;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;
import com.adrninistrator.texttoexcel.entry.TextToExcelEntry;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2025/10/10
 * @description: 获取指定方法被调用时使用的参数类型与值等信息，并记录到文件
 */
public abstract class BaseMethodCallArgInfoRecordHandler extends BaseHandler implements QueryByPageCallBack<WriteDbData4MethodCall> {

    private static final Logger logger = LoggerFactory.getLogger(BaseMethodCallArgInfoRecordHandler.class);

    // 需要处理的被调用类列表
    private final List<String> calleeClassNameList;

    // 结果文本文件路径
    private final String outputTextFilePath;

    // 是否通过文本文件生成Excel文件
    private final boolean genExcel;

    private final WriterSupportHeader writer;

    private final MethodCallInfoHandler methodCallInfoHandler;

    public BaseMethodCallArgInfoRecordHandler(ConfigureWrapper configureWrapper, List<String> calleeClassNameList, String outputTextFilePath, boolean genExcel) {
        super(configureWrapper);
        this.calleeClassNameList = calleeClassNameList;
        this.outputTextFilePath = outputTextFilePath;
        this.genExcel = genExcel;
        String fileHeader = StringUtils.join(chooseFileHeaders(), JavaCG2Constants.FLAG_TAB);
        try {
            writer = new WriterSupportHeader(outputTextFilePath, fileHeader);
        } catch (Exception e) {
            logger.error("error ", e);
            throw new JavaCG2RuntimeException(outputTextFilePath + " " + e.getMessage());
        }
        methodCallInfoHandler = new MethodCallInfoHandler(dbOperWrapper);
        ConfigHandler configHandler = new ConfigHandler(dbOperWrapper);
        // 判断使用 java-callgraph2 组件解析方法调用时是否解析被调用对象和参数可能的类型与值
        if(!configHandler.checkParseMethodCallTypeValue()){
            configHandler.noticeParseMethodCallTypeValue();
        }
    }

    /**
     * 执行
     *
     * @return true: 执行成功 false: 执行失败
     */
    public boolean run() {
        try {
            for (String calleeClassName : calleeClassNameList) {
                String calleeSimpleClassName = dbOperWrapper.querySimpleClassName(calleeClassName);
                // 分页查询并处理
                if (!QueryByPageHandler.queryAndHandle(this, JavaCG2Constants.RECORD_ID_MIN_BEFORE, calleeSimpleClassName)) {
                    return false;
                }
            }

            if (!genExcel) {
                return true;
            }
            IOUtils.closeQuietly(writer);
            // 生成Excel文件，需要先关闭写文本文件流
            int excelWidthPx = configureWrapper.getMainConfig(ConfigKeyEnum.CKE_TEXT_TO_EXCEL_WIDTH_PX);
            TextToExcelEntry textToExcelEntry = new TextToExcelEntry(outputTextFilePath, excelWidthPx, true);
            return textToExcelEntry.convertTextToExcel();
        } finally {
            IOUtils.closeQuietly(writer);
        }
    }

    /**
     * 选择需要生成的文本文件头
     *
     * @return
     */
    protected abstract String[] chooseFileHeaders();

    /**
     * 判断方法调用是否需要处理
     *
     * @param methodCall
     * @param callerMethodDetailNoReturnType
     * @param calleeMethodDetailNoReturnType
     * @return
     */
    protected abstract boolean needHandleMethodCall(WriteDbData4MethodCall methodCall, MethodDetailNoReturnType callerMethodDetailNoReturnType,
                                                    MethodDetailNoReturnType calleeMethodDetailNoReturnType);

    /**
     * 处理方法调用信息
     *
     * @param methodCall
     * @param callerMethodDetailNoReturnType
     * @param calleeMethodDetailNoReturnType
     * @param methodCallInfoMap              外层key: 被调用对象或参数序号，0代表被调用对象，从1开始是参数
     *                                       内层key: 方法调用信息类型，对应 JavaCG2MethodCallInfoTypeEnum 枚举 type 字段值
     *                                       value: 对应类型的数据列表
     */
    protected abstract void handleMethodCallInfo(WriteDbData4MethodCall methodCall, MethodDetailNoReturnType callerMethodDetailNoReturnType,
                                                 MethodDetailNoReturnType calleeMethodDetailNoReturnType, Map<Integer, Map<String, List<String>>> methodCallInfoMap);

    /**
     * 向文件写入数据
     *
     * @param data
     */
    protected void writeData(String... data) {
        try {
            writer.writeDataInLine(data);
        } catch (Exception e) {
            logger.error("error ", e);
            throw new JavaCG2RuntimeException();
        }
    }

    @Override
    public int queryCurrentEndId(int currentStartId, Object... argsByPage) {
        String calleeSimpleClassName = JACGUtil.getArgAt(0, argsByPage);
        return queryEndCallIdEESCNByPage(calleeSimpleClassName, currentStartId);
    }

    @Override
    public List<WriteDbData4MethodCall> queryDataByPage(int currentStartId, int currentEndId, boolean lastQuery, Object... argsByPage) {
        String calleeSimpleClassName = JACGUtil.getArgAt(0, argsByPage);
        SqlKeyEnum sqlKeyEnum = lastQuery ? SqlKeyEnum.MC_QUERY_ALL_BY_CALLEE_CLASS_LIMIT_LAST : SqlKeyEnum.MC_QUERY_ALL_BY_CALLEE_CLASS_LIMIT;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_METHOD_CALL) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName() +
                    " where " + DC.MC_CALLEE_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.MC_CALL_ID + " > ?";
            if (!lastQuery) {
                sql = sql + " and " + DC.MC_CALL_ID + " <= ?";
            }
            sql += " limit ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        List<Object> argList = new ArrayList<>();
        argList.add(calleeSimpleClassName);
        argList.add(currentStartId);
        if (!lastQuery) {
            argList.add(currentEndId);
        }
        argList.add(JACGConstants.DB_PAGE_HANDLE_SIZE);
        return dbOperator.queryList(sql, WriteDbData4MethodCall.class, argList.toArray());
    }

    @Override
    public boolean handleDataList(List<WriteDbData4MethodCall> dataList, Object... argsByPage) throws Exception {
        for (WriteDbData4MethodCall methodCall : dataList) {
            MethodDetailNoReturnType callerMethodDetailNoReturnType = JACGClassMethodUtil.genMethodDetailNoReturnType(methodCall.getCallerFullMethod());
            MethodDetailNoReturnType calleeMethodDetailNoReturnType = JACGClassMethodUtil.genMethodDetailNoReturnType(methodCall.getCalleeFullMethod());
            // 判断方法调用是否需要处理
            if (!needHandleMethodCall(methodCall, callerMethodDetailNoReturnType, calleeMethodDetailNoReturnType)) {
                continue;
            }
            Map<Integer, Map<String, List<String>>> methodCallInfoMap = methodCallInfoHandler.queryMethodCallInfoMapByCallId(methodCall.getCallId());
            // 处理方法调用信息
            handleMethodCallInfo(methodCall, callerMethodDetailNoReturnType, calleeMethodDetailNoReturnType, methodCallInfoMap);
        }
        return true;
    }

    /**
     * 当查询结果为空时是否结束循环查询
     *
     * @return true: 结束 false: 不结束
     */
    @Override
    public boolean exitWhenQueryEmpty() {
        return true;
    }
}
