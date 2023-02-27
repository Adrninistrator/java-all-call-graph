package com.adrninistrator.jacg.handler.method_call_info;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dboper.DbOperator;
import com.adrninistrator.jacg.dto.method_call.MethodCallInfo;
import com.adrninistrator.jacg.dto.method_call.ObjArgsInfoInMethodCall;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg.common.JavaCGConstants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2022/12/9
 * @description: 方法调用信息处理类
 */
public class MethodCallInfoHandler extends BaseHandler {
    private static final Logger logger = LoggerFactory.getLogger(MethodCallInfoHandler.class);

    public MethodCallInfoHandler(DbOperator dbOperator, DbOperWrapper dbOperWrapper) {
        super(dbOperator, dbOperWrapper);
    }

    /**
     * 查询方法调用中被调用对象与参数使用的信息
     *
     * @param callId
     * @return
     */
    public ObjArgsInfoInMethodCall queryObjArgsInfoInMethodCall(int callId) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MCI_QUERY_VALUE;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.joinColumns(DC.MCI_OBJ_ARGS_SEQ, DC.MCI_TYPE, DC.MCI_THE_VALUE) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL_INFO.getTableName(dbOperWrapper.getAppName()) +
                    " where " + DC.MCI_CALL_ID + " = ?" +
                    " order by " + JACGSqlUtil.joinColumns(DC.MCI_OBJ_ARGS_SEQ, DC.MCI_TYPE, DC.MCI_SEQ);
            dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        List<Map<String, Object>> list = dbOperator.queryList(sql, new Object[]{callId});
        if (JACGUtil.isCollectionEmpty(list)) {
            return null;
        }

        ObjArgsInfoInMethodCall objArgsInfoInMethodCall = new ObjArgsInfoInMethodCall();
        for (Map<String, Object> map : list) {
            int objArgsSeq = (int) map.get(DC.MCI_OBJ_ARGS_SEQ);
            String type = (String) map.get(DC.MCI_TYPE);
            String value = (String) map.get(DC.MCI_THE_VALUE);

            if (objArgsSeq == 0) {
                // 当前数据对应被调用对象
                if (objArgsInfoInMethodCall.getObjInfo() == null) {
                    objArgsInfoInMethodCall.setObjInfo(new MethodCallInfo());
                }
                MethodCallInfo objInfo = objArgsInfoInMethodCall.getObjInfo();
                // 添加方法调用信息
                addMethodCallInfo(objInfo, type, value);
                continue;
            }

            // 当前数据对应参数
            // 参数的序号从1开始，为了使参数序号从0开始，将其减1
            int argSeq = objArgsSeq - 1;
            if (objArgsInfoInMethodCall.getArgInfoMap() == null) {
                objArgsInfoInMethodCall.setArgInfoMap(new HashMap<>());
            }
            Map<Integer, MethodCallInfo> argInfoMap = objArgsInfoInMethodCall.getArgInfoMap();
            MethodCallInfo argInfo = argInfoMap.computeIfAbsent(argSeq, k -> new MethodCallInfo());
            // 添加方法调用信息
            addMethodCallInfo(argInfo, type, value);
        }
        return objArgsInfoInMethodCall;
    }

    // 添加方法调用信息
    private void addMethodCallInfo(MethodCallInfo methodCallInfo, String type, String value) {
        List<String> list;
        switch (type) {
            case JavaCGConstants.FILE_KEY_METHOD_CALL_POSSIBLE_INFO_TYPE:
                if (methodCallInfo.getTypeList() == null) {
                    methodCallInfo.setTypeList(new ArrayList<>());
                }
                list = methodCallInfo.getTypeList();
                break;
            case JavaCGConstants.FILE_KEY_METHOD_CALL_POSSIBLE_INFO_VALUE:
            case JavaCGConstants.FILE_KEY_METHOD_CALL_POSSIBLE_INFO_BASE64_VALUE:
                if (methodCallInfo.getValueList() == null) {
                    methodCallInfo.setValueList(new ArrayList<>());
                }
                list = methodCallInfo.getValueList();
                break;
            case JavaCGConstants.FILE_KEY_METHOD_CALL_POSSIBLE_INFO_STATIC_FIELD:
                if (methodCallInfo.getStaticFieldList() == null) {
                    methodCallInfo.setStaticFieldList(new ArrayList<>());
                }
                list = methodCallInfo.getStaticFieldList();
                break;
            case JavaCGConstants.FILE_KEY_METHOD_CALL_POSSIBLE_INFO_STATIC_FIELD_METHOD_CALL:
                if (methodCallInfo.getStaticFieldMethodList() == null) {
                    methodCallInfo.setStaticFieldMethodList(new ArrayList<>());
                }
                list = methodCallInfo.getStaticFieldMethodList();
                break;
            default:
                logger.error("未知类型 {}", type);
                return;
        }
        list.add(value);
    }
}
