package com.adrninistrator.jacg.handler.method;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.method_call.MethodCallInfo;
import com.adrninistrator.jacg.dto.method_call.ObjArgsInfoInMethodCall;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.util.JavaCGUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Collections;
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

    public MethodCallInfoHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
    }

    public MethodCallInfoHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
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
            sql = "select " + JACGSqlUtil.joinColumns(DC.MCI_OBJ_ARGS_SEQ, DC.MCI_SEQ, DC.MCI_TYPE, DC.MCI_THE_VALUE) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL_INFO.getTableName() +
                    " where " + DC.MCI_CALL_ID + " = ?" +
                    " order by " + JACGSqlUtil.joinColumns(DC.MCI_OBJ_ARGS_SEQ, DC.MCI_SEQ);
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        List<Map<String, Object>> list = dbOperator.queryList(sql, new Object[]{callId});
        if (JavaCGUtil.isCollectionEmpty(list)) {
            logger.error("未查询到方法调用中被调用对象与参数使用的信息 {}", callId);
            return null;
        }

        ObjArgsInfoInMethodCall objArgsInfoInMethodCall = new ObjArgsInfoInMethodCall();
        Map<Integer, Map<Integer, MethodCallInfo>> methodCallMapOuter = new HashMap<>();
        // 查询数据库查询结果
        for (Map<String, Object> map : list) {
            int objArgsSeq = (int) map.get(DC.MCI_OBJ_ARGS_SEQ);
            int seq = (int) map.get(DC.MCI_SEQ);
            String type = (String) map.get(DC.MCI_TYPE);
            String value = (String) map.get(DC.MCI_THE_VALUE);
            // 将查询到的数据设置到对应Map中
            Map<Integer, MethodCallInfo> methodCallMapInner = methodCallMapOuter.computeIfAbsent(objArgsSeq, k -> new HashMap<>());
            MethodCallInfo methodCallInfo = methodCallMapInner.computeIfAbsent(seq, k -> new MethodCallInfo());
            addMethodCallInfo(methodCallInfo, type, value);
        }

        // 遍历处理后的方法调用信息，设置到对应的被调用对象和参数中
        for (Map.Entry<Integer, Map<Integer, MethodCallInfo>> entry : methodCallMapOuter.entrySet()) {
            Integer objArgsSeq = entry.getKey();
            Map<Integer, MethodCallInfo> methodCallMapInner = entry.getValue();
            // 将被调用信息按照序号排序，生成list
            List<MethodCallInfo> methodCallInfoList = new ArrayList<>(methodCallMapInner.size());
            List<Integer> seqList = new ArrayList<>(methodCallMapInner.keySet());
            Collections.sort(seqList);
            for (Integer seq : seqList) {
                methodCallInfoList.add(methodCallMapInner.get(seq));
            }

            if (objArgsSeq == 0) {
                // 设置到对应的被调用对象信息
                objArgsInfoInMethodCall.setObjInfo(methodCallInfoList);
                continue;
            }

            // 设置到对应的参数信息
            // 参数的序号从1开始
            if (objArgsInfoInMethodCall.getArgInfoMap() == null) {
                objArgsInfoInMethodCall.setArgInfoMap(new HashMap<>());
            }
            Map<Integer, List<MethodCallInfo>> argInfoMap = objArgsInfoInMethodCall.getArgInfoMap();
            argInfoMap.put(objArgsSeq, methodCallInfoList);
        }
        return objArgsInfoInMethodCall;
    }

    // 添加方法调用信息
    private void addMethodCallInfo(MethodCallInfo methodCallInfo, String type, String value) {
        switch (type) {
            case JavaCGConstants.FILE_KEY_METHOD_CALL_POSSIBLE_INFO_TYPE:
                methodCallInfo.setType(value);
                break;
            case JavaCGConstants.FILE_KEY_METHOD_CALL_POSSIBLE_INFO_VALUE:
            case JavaCGConstants.FILE_KEY_METHOD_CALL_POSSIBLE_INFO_BASE64_VALUE:
                methodCallInfo.setValue(value);
                break;
            case JavaCGConstants.FILE_KEY_METHOD_CALL_POSSIBLE_INFO_STATIC_FIELD:
                methodCallInfo.setStaticField(value);
                break;
            case JavaCGConstants.FILE_KEY_METHOD_CALL_POSSIBLE_INFO_STATIC_FIELD_METHOD_CALL:
                methodCallInfo.setStaticFieldMethod(value);
                break;
            default:
                logger.error("未知类型 {}", type);
                return;
        }
    }
}
