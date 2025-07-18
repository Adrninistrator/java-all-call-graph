package com.adrninistrator.jacg.handler.methodcall;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.methodcall.MethodCallInfo;
import com.adrninistrator.jacg.dto.methodcall.ObjArgsInfoInMethodCall;
import com.adrninistrator.jacg.dto.methodcall.parsed.AbstractMethodCallInfoParsed;
import com.adrninistrator.jacg.dto.methodcall.parsed.MethodCallInfoParsed4Constant;
import com.adrninistrator.jacg.dto.methodcall.parsed.MethodCallInfoParsed4MCReturnCallId;
import com.adrninistrator.jacg.dto.methodcall.parsed.MethodCallInfoParsed4MethodArg;
import com.adrninistrator.jacg.dto.methodcall.parsed.MethodCallInfoParsed4NonStaticField;
import com.adrninistrator.jacg.dto.methodcall.parsed.MethodCallInfoParsed4StaticField;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCallInfo;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGMethodCallInfoUtil;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.common.enums.JavaCG2ConstantTypeEnum;
import com.adrninistrator.javacg2.common.enums.JavaCG2MethodCallInfoTypeEnum;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.apache.commons.lang3.ArrayUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Arrays;
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
     * 查询指定方法调用的信息
     *
     * @param methodCallId
     * @return
     */
    public List<WriteDbData4MethodCallInfo> queryMethodCallInfo(int methodCallId) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MCI_QUERY_ALL_BY_CALL_ID;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_METHOD_CALL_INFO) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL_INFO.getTableName() +
                    " where " + DC.MCI_CALL_ID + " = ?" +
                    " order by " + JACGSqlUtil.joinColumns(DC.MCI_OBJ_ARGS_SEQ, DC.MCI_SEQ);
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryList(sql, WriteDbData4MethodCallInfo.class, methodCallId);
    }

    /**
     * 查询方法调用中被调用对象与参数使用的信息
     *
     * @param methodCallId
     * @return
     */
    public ObjArgsInfoInMethodCall queryObjArgsInfoInMethodCall(int methodCallId) {
        List<WriteDbData4MethodCallInfo> list = queryMethodCallInfo(methodCallId);
        if (JavaCG2Util.isCollectionEmpty(list)) {
            logger.warn("从{}表未查询到方法调用中被调用对象与参数使用的信息 methodCallId: {}", DbTableInfoEnum.DTIE_METHOD_CALL_INFO.getTableNameKeyword(), methodCallId);
            return null;
        }

        ObjArgsInfoInMethodCall objArgsInfoInMethodCall = new ObjArgsInfoInMethodCall();
        Map<Integer, Map<Integer, MethodCallInfo>> methodCallMapOuter = new HashMap<>();
        // 查询数据库查询结果
        for (WriteDbData4MethodCallInfo writeDbData4MethodCallInfo : list) {
            // 对方法调用信息的值进行转换
            JACGMethodCallInfoUtil.transferValue(writeDbData4MethodCallInfo);

            // 将查询到的数据设置到对应Map中
            Map<Integer, MethodCallInfo> methodCallMapInner = methodCallMapOuter.computeIfAbsent(writeDbData4MethodCallInfo.getObjArgsSeq(), k -> new HashMap<>());
            MethodCallInfo methodCallInfo = methodCallMapInner.computeIfAbsent(writeDbData4MethodCallInfo.getSeq(), k -> new MethodCallInfo());
            // 添加方法调用信息
            JACGMethodCallInfoUtil.addMethodCallInfo(methodCallInfo, writeDbData4MethodCallInfo.getType(), writeDbData4MethodCallInfo.getValueType(),
                    writeDbData4MethodCallInfo.getTheValue());
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

    /**
     * 查询在指定的调用方法中，使用了某个方法调用返回值或方法参数作为方法调用的被调用对象的方法调用序号
     *
     * @param callerMethodHash        调用方法完整方法HASH+长度
     * @param byMethodCall            true: 查找使用了某个方法调用返回值的情况 false: 使用了某个方法参数的情况
     * @param methodCallIdOrArgSeq    方法调用序号，或方法参数序号
     * @param beforeOrAfterMethodCall TRUE: 需要查找的数据需要在指定的方法调用序号之前 FALSE: 需要查找的数据需要在指定的方法调用序号之后 可为null
     * @param comparedMethodCallId    指定的方法调用序号，仅当前一个参数非null时有效
     * @return
     */
    public List<Integer> queryCallIdInCaller4ObjByMethodCallArg(String callerMethodHash,
                                                                boolean byMethodCall,
                                                                int methodCallIdOrArgSeq,
                                                                Boolean beforeOrAfterMethodCall,
                                                                int comparedMethodCallId) {
        List<WriteDbData4MethodCallInfo> methodCallInfoList = queryMethodCallInfo4CallerByMethodCallOrArg(callerMethodHash, byMethodCall, methodCallIdOrArgSeq);
        if (JavaCG2Util.isCollectionEmpty(methodCallInfoList)) {
            return null;
        }
        List<Integer> methodCallIdList = new ArrayList<>();
        for (WriteDbData4MethodCallInfo methodCallInfo : methodCallInfoList) {
            if (methodCallInfo.getObjArgsSeq() != JavaCG2Constants.METHOD_CALL_OBJECT_SEQ) {
                // 当前方法调用信息不是作为被调用对象使用
                continue;
            }
            if (Boolean.TRUE.equals(beforeOrAfterMethodCall) && methodCallInfo.getCallId() >= comparedMethodCallId) {
                // 当前查找到的方法调用信息在指定的方法调用序号之后，不处理
                continue;
            }
            if (Boolean.FALSE.equals(beforeOrAfterMethodCall) && methodCallInfo.getCallId() <= comparedMethodCallId) {
                // 当前查找到的方法调用信息在指定的方法调用序号之前，不处理
                continue;
            }
            methodCallIdList.add(methodCallInfo.getCallId());
        }
        return methodCallIdList;
    }

    // 对方法调用中被调用对象或某个参数的信息进行解析
    private List<AbstractMethodCallInfoParsed> parseMethodCallInfo(List<WriteDbData4MethodCallInfo> methodCallInfoList, boolean equivalentConversion) {
        if (JavaCG2Util.isCollectionEmpty(methodCallInfoList)) {
            return null;
        }
        List<AbstractMethodCallInfoParsed> methodCallInfoParsedList = new ArrayList<>();
        // 第一次遍历，向返回列表中加入解析后的对象
        for (int i = 0; i < methodCallInfoList.size(); i++) {
            WriteDbData4MethodCallInfo methodCallInfo = methodCallInfoList.get(i);
            AbstractMethodCallInfoParsed methodCallInfoParsed = null;
            if (JavaCG2MethodCallInfoTypeEnum.MCIT_VALUE.getType().equals(methodCallInfo.getType()) ||
                    JavaCG2MethodCallInfoTypeEnum.MCIT_BASE64_VALUE.getType().equals(methodCallInfo.getType())) {
                methodCallInfoParsed = new MethodCallInfoParsed4Constant();
                ((MethodCallInfoParsed4Constant) methodCallInfoParsed).setConstType(methodCallInfo.getValueType());
                ((MethodCallInfoParsed4Constant) methodCallInfoParsed).setConstValue(methodCallInfo.getTheValue());
            } else if (JavaCG2MethodCallInfoTypeEnum.MCIT_METHOD_ARG_SEQ.getType().equals(methodCallInfo.getType())) {
                // 是否属于等值转换，使用方法参数中对应的值
                methodCallInfoParsed = new MethodCallInfoParsed4MethodArg(equivalentConversion);
                ((MethodCallInfoParsed4MethodArg) methodCallInfoParsed).setMethodArgSeq(Integer.parseInt(methodCallInfo.getTheValue()));
            } else if (JavaCG2MethodCallInfoTypeEnum.MCIT_METHOD_CALL_RETURN_CALL_ID.getType().equals(methodCallInfo.getType())) {
                // 是否属于等值转换，使用方法参数中对应的值
                methodCallInfoParsed = new MethodCallInfoParsed4MCReturnCallId(equivalentConversion);
                ((MethodCallInfoParsed4MCReturnCallId) methodCallInfoParsed).setMethodCallId(Integer.parseInt(methodCallInfo.getTheValue()));
            } else if (JavaCG2MethodCallInfoTypeEnum.MCIT_METHOD_ARG_SEQ_EQC.getType().equals(methodCallInfo.getType())) {
                // 是否属于等值转换，固定使用true
                methodCallInfoParsed = new MethodCallInfoParsed4MethodArg(true);
                ((MethodCallInfoParsed4MethodArg) methodCallInfoParsed).setMethodArgSeq(Integer.parseInt(methodCallInfo.getTheValue()));
            } else if (JavaCG2MethodCallInfoTypeEnum.MCIT_METHOD_CALL_RETURN_CALL_ID_EQC.getType().equals(methodCallInfo.getType())) {
                // 是否属于等值转换，固定使用true
                methodCallInfoParsed = new MethodCallInfoParsed4MCReturnCallId(true);
                ((MethodCallInfoParsed4MCReturnCallId) methodCallInfoParsed).setMethodCallId(Integer.parseInt(methodCallInfo.getTheValue()));
            } else if (JavaCG2MethodCallInfoTypeEnum.MCIT_STATIC_FIELD.getType().equals(methodCallInfo.getType())) {
                methodCallInfoParsed = new MethodCallInfoParsed4StaticField();
                ((MethodCallInfoParsed4StaticField) methodCallInfoParsed).setClassFieldName(methodCallInfo.getTheValue());
            } else if (JavaCG2MethodCallInfoTypeEnum.MCIT_NON_STATIC_FIELD.getType().equals(methodCallInfo.getType())) {
                methodCallInfoParsed = new MethodCallInfoParsed4NonStaticField();
                ((MethodCallInfoParsed4NonStaticField) methodCallInfoParsed).setClassFieldName(methodCallInfo.getTheValue());
            }

            if (methodCallInfoParsed == null) {
                continue;
            }
            // 对解析后的方法调用信息填充信息
            fillInMethodCallInfoParsed(methodCallInfoParsed, methodCallInfoList, methodCallInfo.getSeq());
            methodCallInfoParsedList.add(methodCallInfoParsed);
        }
        return methodCallInfoParsedList;
    }

    // 对解析后的方法调用信息填充信息
    private void fillInMethodCallInfoParsed(AbstractMethodCallInfoParsed methodCallInfoParsed, List<WriteDbData4MethodCallInfo> methodCallInfoList, int seq) {
        if (methodCallInfoParsed instanceof MethodCallInfoParsed4Constant) {
            for (WriteDbData4MethodCallInfo methodCallInfo : methodCallInfoList) {
                if (methodCallInfo.getSeq() != seq) {
                    continue;
                }
                if (JavaCG2MethodCallInfoTypeEnum.MCIT_NAME_OF_VARIABLE.getType().equals(methodCallInfo.getType())) {
                    ((MethodCallInfoParsed4Constant) methodCallInfoParsed).setLocalVariableName(methodCallInfo.getTheValue());
                }
            }
            return;
        }
        if (methodCallInfoParsed instanceof MethodCallInfoParsed4NonStaticField) {
            for (WriteDbData4MethodCallInfo methodCallInfo : methodCallInfoList) {
                if (methodCallInfo.getSeq() != seq) {
                    continue;
                }
                if (JavaCG2MethodCallInfoTypeEnum.MCIT_TYPE.getType().equals(methodCallInfo.getType())) {
                    ((MethodCallInfoParsed4NonStaticField) methodCallInfoParsed).setFieldType(methodCallInfo.getTheValue());
                }
            }
            return;
        }
        if (methodCallInfoParsed instanceof MethodCallInfoParsed4StaticField) {
            for (WriteDbData4MethodCallInfo methodCallInfo : methodCallInfoList) {
                if (methodCallInfo.getSeq() != seq) {
                    continue;
                }
                if (JavaCG2MethodCallInfoTypeEnum.MCIT_TYPE.getType().equals(methodCallInfo.getType())) {
                    ((MethodCallInfoParsed4StaticField) methodCallInfoParsed).setFieldType(methodCallInfo.getTheValue());
                }
            }
            return;
        }
        if (methodCallInfoParsed instanceof MethodCallInfoParsed4MethodArg) {
            MethodCallInfoParsed4MethodArg methodCallInfoParsed4MethodArg = (MethodCallInfoParsed4MethodArg) methodCallInfoParsed;
            for (WriteDbData4MethodCallInfo methodCallInfo : methodCallInfoList) {
                if (methodCallInfo.getSeq() != seq) {
                    continue;
                }
                if (JavaCG2MethodCallInfoTypeEnum.MCIT_NAME_OF_VARIABLE.getType().equals(methodCallInfo.getType())) {
                    methodCallInfoParsed4MethodArg.setMethodArgName(methodCallInfo.getTheValue());
                }
                if (JavaCG2MethodCallInfoTypeEnum.MCIT_TYPE.getType().equals(methodCallInfo.getType())) {
                    methodCallInfoParsed4MethodArg.setMethodArgType(methodCallInfo.getTheValue());
                }
            }
            return;
        }
        if (methodCallInfoParsed instanceof MethodCallInfoParsed4MCReturnCallId) {
            MethodCallInfoParsed4MCReturnCallId methodCallInfoParsed4MCReturnCallId = (MethodCallInfoParsed4MCReturnCallId) methodCallInfoParsed;
            for (WriteDbData4MethodCallInfo methodCallInfo : methodCallInfoList) {
                if (methodCallInfo.getSeq() != seq) {
                    continue;
                }
                if (JavaCG2MethodCallInfoTypeEnum.MCIT_NAME_OF_VARIABLE.getType().equals(methodCallInfo.getType())) {
                    methodCallInfoParsed4MCReturnCallId.setLocalVariableName(methodCallInfo.getTheValue());
                }
                if (JavaCG2MethodCallInfoTypeEnum.MCIT_STATIC_FIELD_MCR.getType().equals(methodCallInfo.getType())) {
                    methodCallInfoParsed4MCReturnCallId.setStaticFieldFullMethod(methodCallInfo.getTheValue());
                }
            }
        }
    }

    /**
     * 查询指定方法调用指定的被调用对象或参数解析后的信息
     *
     * @param methodCallId
     * @param objArgsSeq           0代表被调用对象，1开始为参数
     * @param equivalentConversion true: 属于等值转换前的数据 false: 不属于等值转换
     * @return
     */
    public List<AbstractMethodCallInfoParsed> queryMethodCallInfoParsedObjArg(int methodCallId, int objArgsSeq, boolean equivalentConversion) {
        List<WriteDbData4MethodCallInfo> methodCallInfoList = queryMethodCallInfoObjArg(methodCallId, objArgsSeq);
        return parseMethodCallInfo(methodCallInfoList, equivalentConversion);
    }

    /**
     * 查询指定方法调用指定的被调用对象或参数的信息
     *
     * @param methodCallId
     * @param objArgsSeq   0代表被调用对象，1开始为参数
     * @return
     */
    public List<WriteDbData4MethodCallInfo> queryMethodCallInfoObjArg(int methodCallId, int objArgsSeq) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MCI_QUERY_ALL_BY_CALL_ID_OBJ_ARGS_SEQ;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_METHOD_CALL_INFO) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL_INFO.getTableName() +
                    " where " + DC.MCI_CALL_ID + " = ?" +
                    " and " + DC.MCI_OBJ_ARGS_SEQ + " = ?" +
                    " order by " + DC.MCI_SEQ;
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryList(sql, WriteDbData4MethodCallInfo.class, methodCallId, objArgsSeq);
    }

    /**
     * 查询在指定的调用方法中，使用了某个方法调用返回值或方法参数的情况
     *
     * @param callerMethodHash     调用方法完整方法HASH+长度
     * @param byMethodCall         true: 查找使用了某个方法调用返回值的情况 false: 使用了某个方法参数的情况
     * @param methodCallIdOrArgSeq 方法调用序号，或方法参数序号
     * @return
     */
    public List<WriteDbData4MethodCallInfo> queryMethodCallInfo4CallerByMethodCallOrArg(String callerMethodHash, boolean byMethodCall, int methodCallIdOrArgSeq) {
        if (byMethodCall) {
            return queryMethodCallInfo4CallerByTypeValue(callerMethodHash, String.valueOf(methodCallIdOrArgSeq),
                    JavaCG2MethodCallInfoTypeEnum.MCIT_METHOD_CALL_RETURN_CALL_ID.getType(), JavaCG2MethodCallInfoTypeEnum.MCIT_METHOD_CALL_RETURN_CALL_ID_EQC.getType());
        }
        return queryMethodCallInfo4CallerByTypeValue(callerMethodHash, String.valueOf(methodCallIdOrArgSeq), JavaCG2MethodCallInfoTypeEnum.MCIT_METHOD_ARG_SEQ.getType(),
                JavaCG2MethodCallInfoTypeEnum.MCIT_METHOD_ARG_SEQ_EQC.getType());
    }

    /**
     * 查询在指定的调用方法中，被调用对象或参数中使用了指定类型及值的情况
     *
     * @param callerMethodHash 调用方法完整方法HASH+长度
     * @param value            值
     * @param types            类型，可以指定一种或多种
     * @return
     */
    public List<WriteDbData4MethodCallInfo> queryMethodCallInfo4CallerByTypeValue(String callerMethodHash, String value, String... types) {
        if (ArrayUtils.isEmpty(types)) {
            logger.error("类型参数不允许为空");
            throw new JavaCG2RuntimeException("类型参数不允许为空");
        }
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MCI_QUERY_4_CALLER_BY_TYPE_VALUE;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum, types.length);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_METHOD_CALL_INFO) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL_INFO.getTableName() +
                    " where " + DC.MCI_CALLER_METHOD_HASH + " = ?" +
                    " and " + DC.MCI_TYPE + " in " + JACGSqlUtil.genQuestionString(types.length) +
                    " and " + DC.MCI_THE_VALUE + " = ?" +
                    " order by " + DC.MCI_RECORD_ID;
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql, types.length);
        }
        List<Object> argList = new ArrayList<>(2 + types.length);
        argList.add(callerMethodHash);
        argList.add(value);
        argList.addAll(Arrays.asList(types));
        return dbOperator.queryList(sql, WriteDbData4MethodCallInfo.class, argList.toArray());
    }

    /**
     * 根据方法调用ID、被调用对象或参数序号、序号，及类型，查询方法调用信息
     *
     * @param callId
     * @param objArgsSeq
     * @param seq
     * @param type
     * @return
     */
    public WriteDbData4MethodCallInfo queryMethodCallInfoByCallIdSeqType(int callId, int objArgsSeq, int seq, String type) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MCI_QUERY_BY_MC_ID_SEQ_TYPE;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_METHOD_CALL_INFO) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL_INFO.getTableName() +
                    " where " + DC.MCI_CALL_ID + " = ?" +
                    " and " + DC.MCI_OBJ_ARGS_SEQ + " = ?" +
                    " and " + DC.MCI_SEQ + " = ?" +
                    " and " + DC.MCI_TYPE + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryObject(sql, WriteDbData4MethodCallInfo.class, callId, objArgsSeq, seq, type);
    }

    /**
     * 根据方法调用ID、被调用对象或参数序号，及类型，查询方法调用信息
     *
     * @param callId
     * @param objArgsSeq
     * @param type
     * @return
     */
    public List<WriteDbData4MethodCallInfo> queryMethodCallInfoByCallIdType(int callId, int objArgsSeq, String type) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MCI_QUERY_BY_MC_ID_TYPE;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_METHOD_CALL_INFO) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL_INFO.getTableName() +
                    " where " + DC.MCI_CALL_ID + " = ?" +
                    " and " + DC.MCI_OBJ_ARGS_SEQ + " = ?" +
                    " and " + DC.MCI_TYPE + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryList(sql, WriteDbData4MethodCallInfo.class, callId, objArgsSeq, type);
    }

    /**
     * 查询指定方法的参数的使用情况
     *
     * @param fullMethod 完整方法
     * @param returnType 方法返回类型
     * @param argSeq     参数序号，从1开始
     * @return
     */
    public List<WriteDbData4MethodCallInfo> queryMethodArgUsage(String fullMethod, String returnType, int argSeq) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MCI_QUERY_METHOD_ARG_USAGE;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_METHOD_CALL_INFO) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL_INFO.getTableName() +
                    " where " + DC.MCI_CALLER_METHOD_HASH + " = ?" +
                    " and " + DC.MCI_TYPE + " = ?" +
                    " and " + DC.MCI_THE_VALUE + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        String methodHash = JACGClassMethodUtil.genMethodHashWithLen(fullMethod, returnType);
        return dbOperator.queryList(sql, WriteDbData4MethodCallInfo.class, methodHash, JavaCG2MethodCallInfoTypeEnum.MCIT_METHOD_ARG_SEQ.getType(), String.valueOf(argSeq));
    }

    /**
     * 检查指定方法的参数是否有使用
     *
     * @param fullMethod 完整方法
     * @param returnType 方法返回类型
     * @param argSeq     参数序号，从1开始
     * @return true: 有使用 false: 未使用
     */
    public boolean checkMethodArgUsed(String fullMethod, String returnType, int argSeq) {
        List<WriteDbData4MethodCallInfo> list = queryMethodArgUsage(fullMethod, returnType, argSeq);
        return !JavaCG2Util.isCollectionEmpty(list);
    }

    /**
     * 查询指定方法调用的被调用对象，或参数可能的类型
     *
     * @param callId     方法调用ID
     * @param objArgsSeq 0代表被调用对象，1开始为参数
     * @return
     */
    public List<String> queryMethodCallObjArgTypes(int callId, int objArgsSeq) {
        return queryMethodCallObjArgInfoByType(callId, objArgsSeq, JavaCG2MethodCallInfoTypeEnum.MCIT_TYPE.getType());
    }

    /**
     * 查询指定方法调用的被调用对象，或参数可能的值
     *
     * @param callId     方法调用ID
     * @param objArgsSeq 0代表被调用对象，1开始为参数
     * @return
     */
    public List<String> queryMethodCallObjArgValues(int callId, int objArgsSeq) {
        return queryMethodCallObjArgInfoByType(callId, objArgsSeq, JavaCG2MethodCallInfoTypeEnum.MCIT_VALUE.getType());
    }

    /**
     * 查询指定方法调用的被调用对象，或参数的指定类型的信息
     *
     * @param callId     方法调用ID
     * @param objArgsSeq 0代表被调用对象，1开始为参数
     * @param type       类型
     * @return
     */
    private List<String> queryMethodCallObjArgInfoByType(int callId, int objArgsSeq, String type) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MCI_QUERY_OBJ_ARG_INFO_BY_TYPE;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select distinct " + DC.MCI_THE_VALUE +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL_INFO.getTableName() +
                    " where " + DC.MCI_CALL_ID + " = ?" +
                    " and " + DC.MCI_OBJ_ARGS_SEQ + " = ?" +
                    " and " + DC.MCI_TYPE + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryListOneColumn(sql, String.class, callId, objArgsSeq, type);
    }

    /**
     * 根据常量类型与常量值，查询被调用对象或参数使用指定常量值的方法调用信息
     *
     * @param javaCG2ConstantTypeEnum 常量类型枚举
     * @param value                   常量值，不管哪种常量类型，都传入对应值的字符串形式
     * @return
     */
    public List<WriteDbData4MethodCallInfo> queryConstValueMCIByType(JavaCG2ConstantTypeEnum javaCG2ConstantTypeEnum, String value) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MCI_QUERY_CONST_VALUE_BY_TYPE;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_METHOD_CALL_INFO) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL_INFO.getTableName() +
                    " where " + DC.MCI_TYPE + " = ?" +
                    " and " + DC.MCI_VALUE_TYPE + " = ?" +
                    " and " + DC.MCI_THE_VALUE + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryList(sql, WriteDbData4MethodCallInfo.class, JavaCG2MethodCallInfoTypeEnum.MCIT_VALUE.getType(), javaCG2ConstantTypeEnum.getType(), value);
    }

    /**
     * 查询指定方法中指定被调用对象或参数对应的方法调用信息
     *
     * @param callId     方法调用ID
     * @param objArgSeqs 需要查询的被调用对象或参数序号，若为空则查询全部被调用对象及参数，0代表被调用对象，1开始为参数
     * @return
     */
    public List<WriteDbData4MethodCallInfo> queryMethodCallInfoObjArgs(int callId, int... objArgSeqs) {
        int objArgSeqNum = ArrayUtils.isEmpty(objArgSeqs) ? 0 : objArgSeqs.length;
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MCI_QUERY_ARG_VALUES;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum, objArgSeqNum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.joinColumns(DC.MCI_OBJ_ARGS_SEQ, DC.MCI_TYPE, DC.MCI_THE_VALUE) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL_INFO.getTableName() +
                    " where " + DC.MCI_CALL_ID + " = ?";
            if (objArgSeqNum > 0) {
                sql += " and " + DC.MCI_OBJ_ARGS_SEQ + " in " + JACGSqlUtil.genQuestionString(objArgSeqNum);
            }
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        List<Object> argList = new ArrayList<>(1 + objArgSeqNum);
        argList.add(callId);
        if (objArgSeqNum > 0) {
            for (int argSeq : objArgSeqs) {
                argList.add(argSeq);
            }
        }
        return dbOperator.queryList(sql, WriteDbData4MethodCallInfo.class, argList.toArray());
    }
}
