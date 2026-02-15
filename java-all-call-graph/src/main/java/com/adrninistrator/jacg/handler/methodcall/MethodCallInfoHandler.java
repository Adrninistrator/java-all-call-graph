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
import com.adrninistrator.jacg.handler.dto.methodcall.MethodCallObjArgValueAndSource;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGMethodCallInfoUtil;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.common.enums.JavaCG2ConstantTypeEnum;
import com.adrninistrator.javacg2.common.enums.JavaCG2MethodCallInfoTypeEnum;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2022/12/9
 * @description: 方法调用信息处理类
 */
public class MethodCallInfoHandler extends BaseHandler {
    private static final Logger logger = LoggerFactory.getLogger(MethodCallInfoHandler.class);

    // TYPE字段名称
    private static final String FIELD_NAME_TYPE = "TYPE";

    // 包装类型到原始类型的映射
    private static final Map<String, String> WRAPPER_TO_PRIMITIVE_MAP = new HashMap<>();

    // 最终的静态字段到原始类型映射（用于查询转换）
    private static final Map<String, String> STATIC_FIELD_TO_PRIMITIVE_MAP = new HashMap<>();

    static {
        // 初始化包装类型到原始类型的映射
        WRAPPER_TO_PRIMITIVE_MAP.put(Integer.class.getName(), int.class.getName());
        WRAPPER_TO_PRIMITIVE_MAP.put(Long.class.getName(), long.class.getName());
        WRAPPER_TO_PRIMITIVE_MAP.put(Short.class.getName(), short.class.getName());
        WRAPPER_TO_PRIMITIVE_MAP.put(Byte.class.getName(), byte.class.getName());
        WRAPPER_TO_PRIMITIVE_MAP.put(Character.class.getName(), char.class.getName());
        WRAPPER_TO_PRIMITIVE_MAP.put(Boolean.class.getName(), boolean.class.getName());
        WRAPPER_TO_PRIMITIVE_MAP.put(Float.class.getName(), float.class.getName());
        WRAPPER_TO_PRIMITIVE_MAP.put(Double.class.getName(), double.class.getName());
        WRAPPER_TO_PRIMITIVE_MAP.put(Void.class.getName(), void.class.getName());

        // 根据包装类型映射生成最终的静态字段到原始类型映射
        for (Map.Entry<String, String> entry : WRAPPER_TO_PRIMITIVE_MAP.entrySet()) {
            String wrapperClassName = entry.getKey();
            String primitiveTypeName = entry.getValue();
            String staticFieldKey = JavaCG2ClassMethodUtil.formatClassAndField(wrapperClassName, FIELD_NAME_TYPE);
            STATIC_FIELD_TO_PRIMITIVE_MAP.put(staticFieldKey, primitiveTypeName);
        }
    }

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
                    " and " + DC.MCI_THE_VALUE + " = ?" +
                    " and " + DC.MCI_TYPE + " in " + JACGSqlUtil.genQuestionString(types.length) +
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
     * 查询指定方法中使用指定序号的参数作为方法调用被调用对象的方法调用序号
     *
     * @param callerMethodHash
     * @param argSeq
     * @return
     */
    public List<Integer> queryCallId4MethodCalleeObjUseArg(String callerMethodHash, int argSeq) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MCI_QUERY_CALL_ID_4_METHOD_CALLEE_OBJ_USE_ARG;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + DC.MCI_CALL_ID +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL_INFO.getTableName() +
                    " where " + DC.MCI_CALLER_METHOD_HASH + " = ?" +
                    " and " + DC.MCI_OBJ_ARGS_SEQ + " = ? " +
                    " and " + DC.MCI_TYPE + " = ? " +
                    " and " + DC.MCI_THE_VALUE + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryListOneColumn(sql, Integer.class, callerMethodHash, JavaCG2Constants.METHOD_CALL_OBJECT_SEQ,
                JavaCG2MethodCallInfoTypeEnum.MCIT_METHOD_ARG_SEQ.getType(), String.valueOf(argSeq));
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
    public List<String> queryMethodCallInfoByCallIdType(int callId, int objArgsSeq, String type) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MCI_QUERY_BY_MC_ID_TYPE;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + DC.MCI_THE_VALUE +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL_INFO.getTableName() +
                    " where " + DC.MCI_CALL_ID + " = ?" +
                    " and " + DC.MCI_OBJ_ARGS_SEQ + " = ?" +
                    " and " + DC.MCI_TYPE + " = ?" +
                    " order by " + DC.MCI_SEQ;
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryListOneColumn(sql, String.class, callId, objArgsSeq, type);
    }

    /**
     * 根据方法调用ID，查询方法调用信息
     *
     * @param callId
     * @return
     */
    public List<WriteDbData4MethodCallInfo> queryMethodCallInfoByCallId(int callId) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MCI_QUERY_BY_MC_ID;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_METHOD_CALL_INFO) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL_INFO.getTableName() +
                    " where " + DC.MCI_CALL_ID + " = ?" +
                    " order by " + DC.MCI_OBJ_ARGS_SEQ + ", " + DC.MCI_SEQ;
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryList(sql, WriteDbData4MethodCallInfo.class, callId);
    }

    /**
     * 根据方法调用ID，查询方法调用信息，Map格式
     *
     * @param callId
     * @return 外层key: 被调用对象或参数序号，0代表被调用对象，从1开始是参数
     * 内层key: 方法调用信息类型，对应 JavaCG2MethodCallInfoTypeEnum 枚举 type 字段值
     * value: 对应类型的数据列表
     */
    public Map<Integer, Map<String, List<String>>> queryMethodCallInfoMapByCallId(int callId) {
        Map<Integer, Map<String, List<String>>> map = new HashMap<>();
        List<WriteDbData4MethodCallInfo> list = queryMethodCallInfoByCallId(callId);
        if (JavaCG2Util.isCollectionEmpty(list)) {
            return map;
        }
        for (WriteDbData4MethodCallInfo methodCallInfo : list) {
            Map<String, List<String>> innerMap = map.computeIfAbsent(methodCallInfo.getObjArgsSeq(), k -> new HashMap<>());
            List<String> valueList = innerMap.computeIfAbsent(methodCallInfo.getType(), k -> new ArrayList<>());
            valueList.add(methodCallInfo.getTheValue());
        }
        return map;
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
     * 查询指定方法调用的参数类型
     * 对于Class.getMethod等方法的参数类型，需要处理两种情况：
     * 1. 非基本类型（如String）：type="t"，theValue为参数类型字符串
     * 2. 基本类型（如int）：type="sf"，theValue为基本类型对应的包装类型中的TYPE字段，如"java.lang.Integer:TYPE"
     *
     * @param callId     方法调用ID
     * @param objArgsSeq 参数序号，1开始
     * @return 参数类型列表，基本类型会转换为对应的基本类型名称（如int）
     */
    public List<String> queryMethodCallArgTypes(int callId, int objArgsSeq) {
        List<String> resultList = new ArrayList<>();

        // 查询非基本类型（type="t"）
        List<String> typeList = queryMethodCallObjArgInfoByType(callId, objArgsSeq, JavaCG2MethodCallInfoTypeEnum.MCIT_TYPE.getType());
        if (!JavaCG2Util.isCollectionEmpty(typeList)) {
            resultList.addAll(typeList);
        }

        // 查询基本类型（type="sf"）
        List<String> staticFieldList = queryMethodCallObjArgInfoByType(callId, objArgsSeq, JavaCG2MethodCallInfoTypeEnum.MCIT_STATIC_FIELD.getType());
        if (!JavaCG2Util.isCollectionEmpty(staticFieldList)) {
            for (String staticField : staticFieldList) {
                String primitiveType = convertStaticFieldToPrimitiveType(staticField);
                if (StringUtils.isNotBlank(primitiveType) && !resultList.contains(primitiveType)) {
                    resultList.add(primitiveType);
                }
            }
        }

        return resultList;
    }

    /**
     * 将静态字段值转换为基本类型名称
     * 例如："java.lang.Integer:TYPE" -> "int"
     *
     * @param staticField 静态字段值
     * @return 基本类型名称，如果不是基本类型对应的静态字段则返回原值
     */
    private String convertStaticFieldToPrimitiveType(String staticField) {
        if (StringUtils.isBlank(staticField)) {
            return null;
        }

        String primitiveType = STATIC_FIELD_TO_PRIMITIVE_MAP.get(staticField);
        if (primitiveType != null) {
            return primitiveType;
        }

        // 如果不是基本类型对应的TYPE字段，返回原值
        return staticField;
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

    /**
     * 查询方法调用中被调用对象与参数对应的常量值或来源（包括使用方法调用的返回值，或方法参数）
     *
     * @param callId    方法调用ID
     * @param objArgSeq 被调用对象或参数序号，0代表被调用对象，1开始为参数
     * @return
     */
    public MethodCallObjArgValueAndSource queryMethodCallObjArgValueAndSource(int callId, int objArgSeq) {
        MethodCallObjArgValueAndSource methodCallObjArgValueAndSource = new MethodCallObjArgValueAndSource();
        Set<String> methodCallInfoTypeSet = methodCallObjArgValueAndSource.getMethodCallInfoTypeSet();
        // 查询指定方法调用指定的被调用对象或参数的信息
        List<WriteDbData4MethodCallInfo> methodCallInfoList = queryMethodCallInfoObjArg(callId, objArgSeq);
        if (JavaCG2Util.isCollectionEmpty(methodCallInfoList)) {
            logger.warn("通过方法调用ID未查询到对应的方法调用信息 {}", callId);
            return methodCallObjArgValueAndSource;
        }

        Set<String> typeSet = new HashSet<>();
        // 对应的常量值列表
        List<String> constantValueList = new ArrayList<>();
        // 对应的使用方法调用的返回值的方法调用ID列表
        List<Integer> useMethodCallReturnCallIdList = new ArrayList<>();
        // 对应的使用方法参数序号列表
        List<Integer> useMethodArgSeqList = new ArrayList<>();
        methodCallObjArgValueAndSource.setConstantValueList(constantValueList);
        methodCallObjArgValueAndSource.setUseMethodCallReturnCallIdList(useMethodCallReturnCallIdList);
        methodCallObjArgValueAndSource.setUseMethodArgSeqList(useMethodArgSeqList);

        for (WriteDbData4MethodCallInfo methodCallInfo : methodCallInfoList) {
            if (JavaCG2MethodCallInfoTypeEnum.MCIT_VALUE.getType().equals(methodCallInfo.getType())) {
                typeSet.add(methodCallInfo.getType());
                constantValueList.add(methodCallInfo.getTheValue());
                methodCallInfoTypeSet.add(JavaCG2MethodCallInfoTypeEnum.MCIT_VALUE.getType());
            } else if (JavaCG2MethodCallInfoTypeEnum.MCIT_METHOD_CALL_RETURN_CALL_ID.getType().equals(methodCallInfo.getType())) {
                typeSet.add(methodCallInfo.getType());
                useMethodCallReturnCallIdList.add(Integer.valueOf(methodCallInfo.getTheValue()));
                methodCallInfoTypeSet.add(JavaCG2MethodCallInfoTypeEnum.MCIT_METHOD_CALL_RETURN_CALL_ID.getType());
            } else if (JavaCG2MethodCallInfoTypeEnum.MCIT_METHOD_ARG_SEQ.getType().equals(methodCallInfo.getType())) {
                typeSet.add(methodCallInfo.getType());
                useMethodArgSeqList.add(Integer.valueOf(methodCallInfo.getTheValue()));
                methodCallInfoTypeSet.add(JavaCG2MethodCallInfoTypeEnum.MCIT_METHOD_ARG_SEQ.getType());
            }
        }
        return methodCallObjArgValueAndSource;
    }

    /**
     * 查询指定方法调用的被调用对象或方法参数对应的数组元素类型
     * 同时处理非基本类型（type="t"）和基本类型（type="sf"）
     * 基本类型会自动转换：如 "java.lang.Integer:TYPE" -> "int"
     *
     * @param callId     方法调用ID
     * @param objArgsSeq 0代表被调用对象，1开始为参数
     * @return List中的元素代表数组元素结果的不同的集合，Map表示数组元素的序号及对应类型
     *         Map的key为arrayIndex解析后的整数值，value为类型字符串
     */
    public List<Map<Integer, String>> queryMethodCallArrayElementTypes(int callId, int objArgsSeq) {
        // 一次查询 type="t" 和 type="sf" 的数据
        List<WriteDbData4MethodCallInfo> allDataList = queryMethodCallArrayElementData4Types(callId, objArgsSeq);

        if (JavaCG2Util.isCollectionEmpty(allDataList)) {
            return Collections.emptyList();
        }

        // 按array_collection_seq分组，直接使用一个Map记录，key使用arrayIndex解析后的整数值
        Map<Integer, Map<Integer, String>> groupedMap = new HashMap<>();

        for (WriteDbData4MethodCallInfo data : allDataList) {
            int collectionSeq = data.getArrayCollectionSeq();
            int arrayIndexInt = Integer.parseInt(data.getArrayIndex());

            Map<Integer, String> elementMap = groupedMap.computeIfAbsent(collectionSeq, k -> new HashMap<>());

            if (JavaCG2MethodCallInfoTypeEnum.MCIT_TYPE.getType().equals(data.getType())) {
                // 处理非基本类型数据
                elementMap.put(arrayIndexInt, data.getTheValue());
            } else if (JavaCG2MethodCallInfoTypeEnum.MCIT_STATIC_FIELD.getType().equals(data.getType())) {
                // 处理基本类型数据，转换为原始类型
                String primitiveType = convertStaticFieldToPrimitiveType(data.getTheValue());
                if (StringUtils.isNotBlank(primitiveType)) {
                    elementMap.put(arrayIndexInt, primitiveType);
                }
            }
        }

        // 转换为List格式并按collectionSeq排序
        List<Map<Integer, String>> resultList = new ArrayList<>();
        List<Integer> collectionSeqList = new ArrayList<>(groupedMap.keySet());
        Collections.sort(collectionSeqList);

        for (Integer collectionSeq : collectionSeqList) {
            Map<Integer, String> elementMap = groupedMap.get(collectionSeq);
            if (!elementMap.isEmpty()) {
                resultList.add(elementMap);
            }
        }

        return resultList;
    }

    /**
     * 查询指定方法调用的被调用对象或方法参数对应的数组元素值
     *
     * @param callId     方法调用ID
     * @param objArgsSeq 0代表被调用对象，1开始为参数
     * @return List中的元素代表数组元素结果的不同的集合，Map表示数组元素的序号及对应值
     *         Map的key为arrayIndex解析后的整数值，value为对应值
     */
    public List<Map<Integer, String>> queryMethodCallArrayElementValues(int callId, int objArgsSeq) {
        return queryMethodCallArrayElementInfo(callId, objArgsSeq, JavaCG2MethodCallInfoTypeEnum.MCIT_VALUE.getType());
    }

    /**
     * 查询指定方法调用的被调用对象或方法参数对应的数组元素信息
     *
     * @param callId     方法调用ID
     * @param objArgsSeq 0代表被调用对象，1开始为参数
     * @param type       信息类型，如类型(t)或值等
     * @return List中的元素代表数组元素结果的不同的集合，Map表示数组元素的序号及对应信息
     *         Map的key为arrayIndex解析后的整数值，value为对应信息
     */
    public List<Map<Integer, String>> queryMethodCallArrayElementInfo(int callId, int objArgsSeq, String type) {
        List<WriteDbData4MethodCallInfo> methodCallInfoList = queryMethodCallArrayElementData(callId, objArgsSeq, type);
        if (JavaCG2Util.isCollectionEmpty(methodCallInfoList)) {
            return Collections.emptyList();
        }

        // 按array_collection_seq分组
        Map<Integer, Map<Integer, String>> groupedMap = new HashMap<>();
        for (WriteDbData4MethodCallInfo methodCallInfo : methodCallInfoList) {
            int arrayCollectionSeq = methodCallInfo.getArrayCollectionSeq();
            int arrayIndexInt = Integer.parseInt(methodCallInfo.getArrayIndex());
            groupedMap.computeIfAbsent(arrayCollectionSeq, k -> new HashMap<>()).put(arrayIndexInt, methodCallInfo.getTheValue());
        }

        // 转换为List<Map<Integer, String>>格式
        List<Map<Integer, String>> resultList = new ArrayList<>();
        List<Integer> collectionSeqList = new ArrayList<>(groupedMap.keySet());
        Collections.sort(collectionSeqList);

        for (Integer collectionSeq : collectionSeqList) {
            Map<Integer, String> elementMap = groupedMap.get(collectionSeq);
            if (!elementMap.isEmpty()) {
                resultList.add(elementMap);
            }
        }

        return resultList;
    }

    /**
     * 查询指定方法调用的被调用对象或方法参数对应的数组元素原始数据
     * 按array_collection_seq、array_dimensions、array_index排序
     *
     * @param callId     方法调用ID
     * @param objArgsSeq 0代表被调用对象，1开始为参数
     * @param type       信息类型
     * @return 数组元素的原始数据列表
     */
    private List<WriteDbData4MethodCallInfo> queryMethodCallArrayElementData(int callId, int objArgsSeq, String type) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MCI_QUERY_ARRAY_ELEMENT_INFO;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_METHOD_CALL_INFO) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL_INFO.getTableName() +
                    " where " + DC.MCI_CALL_ID + " = ?" +
                    " and " + DC.MCI_OBJ_ARGS_SEQ + " = ?" +
                    " and " + DC.MCI_TYPE + " = ?" +
                    " and " + DC.MCI_ARRAY_FLAG + " = 1" +
                    " order by " + DC.MCI_ARRAY_COLLECTION_SEQ + ", " + DC.MCI_ARRAY_DIMENSIONS + ", " + DC.MCI_ARRAY_INDEX;
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryList(sql, WriteDbData4MethodCallInfo.class, callId, objArgsSeq, type);
    }

    /**
     * 查询指定方法调用的被调用对象或方法参数对应的数组元素类型数据
     * 一次查询 type="t" 和 type="sf" 的数据
     * 按array_collection_seq、array_dimensions、array_index排序
     *
     * @param callId     方法调用ID
     * @param objArgsSeq 0代表被调用对象，1开始为参数
     * @return 数组元素的原始数据列表
     */
    private List<WriteDbData4MethodCallInfo> queryMethodCallArrayElementData4Types(int callId, int objArgsSeq) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MCI_QUERY_ARRAY_ELEMENT_INFO_4_TYPES;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_METHOD_CALL_INFO) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL_INFO.getTableName() +
                    " where " + DC.MCI_CALL_ID + " = ?" +
                    " and " + DC.MCI_OBJ_ARGS_SEQ + " = ?" +
                    " and " + DC.MCI_TYPE + " in ('" + JavaCG2MethodCallInfoTypeEnum.MCIT_TYPE.getType() + "', '" + JavaCG2MethodCallInfoTypeEnum.MCIT_STATIC_FIELD.getType() + "')" +
                    " and " + DC.MCI_ARRAY_FLAG + " = 1" +
                    " order by " + DC.MCI_ARRAY_COLLECTION_SEQ + ", " + DC.MCI_ARRAY_DIMENSIONS + ", " + DC.MCI_ARRAY_INDEX;
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryList(sql, WriteDbData4MethodCallInfo.class, callId, objArgsSeq);
    }
}
