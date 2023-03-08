package com.adrninistrator.jacg.handler.write_db;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.MethodCallFlagsEnum;
import com.adrninistrator.jacg.dto.method.MethodCallFullMethod;
import com.adrninistrator.jacg.dto.method.MethodDetail;
import com.adrninistrator.jacg.dto.method_call.ObjArgsInfoInMethodCall;
import com.adrninistrator.jacg.dto.write_db.WriteDbData4ExtendedData;
import com.adrninistrator.jacg.dto.write_db.WriteDbData4MethodCall;
import com.adrninistrator.jacg.extensions.dto.extened_data.BaseExtendedData;
import com.adrninistrator.jacg.extensions.extended_data_add.ExtendedDataAddInterface;
import com.adrninistrator.jacg.extensions.method_call_add.MethodCallAddInterface;
import com.adrninistrator.jacg.handler.method_call_info.MethodCallInfoHandler;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.common.enums.JavaCGCallTypeEnum;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2022/11/18
 * @description: 写入数据库，方法调用关系
 */
public class WriteDbHandler4MethodCall extends AbstractWriteDbHandler<WriteDbData4MethodCall> {
    private static final Logger logger = LoggerFactory.getLogger(WriteDbHandler4MethodCall.class);

    // 写入数据库，方法调用自定义数据
    private WriteDbHandler4ExtendedData writeDbHandler4ExtendedData;

    // 方法调用信息处理类
    private MethodCallInfoHandler methodCallInfoHandler;

    // Spring Controller对应的方法HASH+长度
    private Set<String> springControllerMethodHashSet = new HashSet<>();

    // 有注解的方法HASH+长度
    private Set<String> withAnnotationMethodHashSet = new HashSet<>();

    // 被调用对象及参数存在信息的call_id
    private Set<Integer> withInfoCallIdSet = new HashSet<>();

    // 存在方法调用自定义数据的call_id
    private final Set<Integer> withExtendedDataCallIdSet = new HashSet<>();

    // 人工添加方法调用关系类列表
    private List<MethodCallAddInterface> methodCallAddExtList;

    // 保存用于根据方法调用信息添加方法调用自定义数据的处理类
    private List<ExtendedDataAddInterface> extendedDataAddExtList;

    // 人工添加的方法调用关系
    private final List<MethodCallFullMethod> manualAddMethodCallList = new ArrayList<>(batchSize);

    // 根据方法调用信息添加的方法调用自定义数据
    private final List<WriteDbData4ExtendedData> extendedDataList = new ArrayList<>(batchSize);

    @Override
    public void init() {
        methodCallInfoHandler = new MethodCallInfoHandler(dbOperator, dbOperWrapper);
    }

    @Override
    protected WriteDbData4MethodCall genData(String line) {
        String[] array = splitEquals(line, 6);

        int callId = Integer.parseInt(array[0]);
        String callerFullMethod = array[1];
        String tmpCalleeFullMethod = array[2];
        int callerLineNum = Integer.parseInt(array[3]);
        String calleeObjType = array[4];
        String callerJarNum = array[5];

        int indexCalleeLeftBracket = tmpCalleeFullMethod.indexOf(JavaCGConstants.FILE_KEY_CALL_TYPE_FLAG1);
        int indexCalleeRightBracket = tmpCalleeFullMethod.indexOf(JavaCGConstants.FILE_KEY_CALL_TYPE_FLAG2);

        String callType = tmpCalleeFullMethod.substring(indexCalleeLeftBracket + JavaCGConstants.FILE_KEY_CALL_TYPE_FLAG1.length(), indexCalleeRightBracket);
        String calleeFullMethod = tmpCalleeFullMethod.substring(indexCalleeRightBracket + JavaCGConstants.FILE_KEY_CALL_TYPE_FLAG2.length()).trim();

        // 根据完整方法前缀判断是否需要处理
        if (!isAllowedClassPrefix(callerFullMethod) && !isAllowedClassPrefix(calleeFullMethod)) {
            return null;
        }

        String callerClassName = JACGClassMethodUtil.getClassNameFromMethod(callerFullMethod);
        String calleeClassName = JACGClassMethodUtil.getClassNameFromMethod(calleeFullMethod);
        WriteDbData4MethodCall writeDbData4MethodCall = WriteDbData4MethodCall.genInstance(callType,
                calleeObjType,
                dbOperWrapper.getSimpleClassName(callerClassName),
                callerFullMethod,
                dbOperWrapper.getSimpleClassName(calleeClassName),
                calleeFullMethod,
                callId,
                callerLineNum,
                callerJarNum
        );

        if (writeDbData4MethodCall.getCallerMethodHash().equals(writeDbData4MethodCall.getCalleeMethodHash())) {
            // 对于递归调用，不写入数据库，防止查询时出现死循环
            logger.debug("递归调用不写入数据库 {}", line);
            return null;
        }

        // 人工添加方法调用关系
        for (MethodCallAddInterface methodCallAddExt : methodCallAddExtList) {
            MethodCallFullMethod methodCallFullMethod = methodCallAddExt.handleMethodCall(callerFullMethod, calleeFullMethod, calleeClassName);
            if (methodCallFullMethod != null) {
                manualAddMethodCallList.add(methodCallFullMethod);
            }
        }

        if (extendedDataAddExtList != null) {
            /*
                存在根据方法调用信息添加方法调用自定义数据的处理类
                这里不判断withInfoCallIdSet.contains(callId)，因为MyBatis的Mapper被调用时，不存在方法调用信息
             */
            for (ExtendedDataAddInterface extendedDataAddExt : extendedDataAddExtList) {
                MethodDetail calleeMethodDetail = JACGClassMethodUtil.genMethodDetail(calleeFullMethod);
                if (!extendedDataAddExt.checkNeedHandle(callType, calleeMethodDetail)) {
                    // 当前被调用方法不需要进行方法调用自定义数据处理
                    continue;
                }

                // 当前被调用方法需要进行方法调用自定义数据处理
                // 查询方法调用中被调用对象与参数使用的信息
                ObjArgsInfoInMethodCall objArgsInfoInMethodCall = methodCallInfoHandler.queryObjArgsInfoInMethodCall(callId);
                // 生成方法调用自定义数据，以下传入的objArgsInfoInMethodCall对象可能为null
                BaseExtendedData extendedData = extendedDataAddExt.genBaseExtendedData(callType, calleeMethodDetail, objArgsInfoInMethodCall);
                if (extendedData == null) {
                    continue;
                }

                // 记录存在方法调用自定义数据的call_id
                withExtendedDataCallIdSet.add(callId);

                // 尝试将方法调用自定义数据写入数据库
                WriteDbData4ExtendedData writeDbData4ExtendedData = new WriteDbData4ExtendedData(callId, extendedData.getDataType(), extendedData.getDataValue());
                extendedDataList.add(writeDbData4ExtendedData);
                writeDbHandler4ExtendedData.tryInsertDb(extendedDataList);
            }
        }

        // 生成方法调用标记
        int callFlags = genCallFlags(callId, writeDbData4MethodCall.getCallerMethodHash(), writeDbData4MethodCall.getCalleeMethodHash());
        writeDbData4MethodCall.setCallFlags(callFlags);

        return writeDbData4MethodCall;
    }

    @Override
    protected DbTableInfoEnum chooseDbTableInfo() {
        return DbTableInfoEnum.DTIE_METHOD_CALL;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4MethodCall data) {
        return JACGUtil.genMethodCallObjectArray(data);
    }

    @Override
    protected void beforeDone() {
        // 将剩余方法调用自定义数据写入数据库
        writeDbHandler4ExtendedData.insertDb(extendedDataList);
    }

    /**
     * 生成方法调用标记
     *
     * @param callId
     * @param callerMethodHash
     * @param calleeMethodHash
     * @return
     */
    private int genCallFlags(int callId, String callerMethodHash, String calleeMethodHash) {
        int callFlags = 0;
        if (springControllerMethodHashSet.contains(callerMethodHash)) {
            callFlags = MethodCallFlagsEnum.MCFE_ER_SPRING_CONTROLLER.setFlag(callFlags);
        }
        if (withAnnotationMethodHashSet.contains(callerMethodHash)) {
            callFlags = MethodCallFlagsEnum.MCFE_ER_METHOD_ANNOTATION.setFlag(callFlags);
        }
        if (withAnnotationMethodHashSet.contains(calleeMethodHash)) {
            callFlags = MethodCallFlagsEnum.MCFE_EE_METHOD_ANNOTATION.setFlag(callFlags);
        }
        if (withInfoCallIdSet.contains(callId)) {
            callFlags = MethodCallFlagsEnum.MCFE_METHOD_CALL_INFO.setFlag(callFlags);
        }
        if (withExtendedDataCallIdSet.contains(callId)) {
            callFlags = MethodCallFlagsEnum.MCFE_EXTENDED_DATA.setFlag(callFlags);
        }
        return callFlags;
    }

    // 人工添加方法调用关系
    public boolean manualAddMethodCall() {
        if (manualAddMethodCallList.isEmpty()) {
            logger.info("没有人工添加方法调用关系");
        }
        logger.info("人工添加方法调用关系数量 {}", manualAddMethodCallList.size());

        // 查询当前方法调用的最大call_id
        int maxCallId = dbOperWrapper.getMaxMethodCallId();
        if (maxCallId == JACGConstants.MAX_METHOD_CALL_ID_ILLEGAL) {
            return false;
        }

        List<WriteDbData4MethodCall> writeDbData4MethodCallList = new ArrayList<>(batchSize);
        for (MethodCallFullMethod manualAddMethodCall : manualAddMethodCallList) {
            String callerFullMethod = manualAddMethodCall.getCallerFullMethod();
            String calleeFullMethod = manualAddMethodCall.getCalleeFullMethod();
            String callerClassName = JACGClassMethodUtil.getClassNameFromMethod(callerFullMethod);
            String calleeClassName = JACGClassMethodUtil.getClassNameFromMethod(calleeFullMethod);
            WriteDbData4MethodCall writeDbData4MethodCall = WriteDbData4MethodCall.genInstance(JavaCGCallTypeEnum.CTE_MANUAL_ADDED.getType(),
                    "",
                    dbOperWrapper.getSimpleClassName(callerClassName),
                    callerFullMethod,
                    dbOperWrapper.getSimpleClassName(calleeClassName),
                    calleeFullMethod,
                    ++maxCallId,
                    JavaCGConstants.DEFAULT_LINE_NUMBER,
                    String.valueOf(0)
            );
            writeDbData4MethodCallList.add(writeDbData4MethodCall);
            // 尝试写入数据库
            tryInsertDb(writeDbData4MethodCallList);
        }
        // 将剩余内容写入数据库
        insertDb(writeDbData4MethodCallList);
        return true;
    }

    //
    public void setWriteDbHandler4ExtendedData(WriteDbHandler4ExtendedData writeDbHandler4ExtendedData) {
        this.writeDbHandler4ExtendedData = writeDbHandler4ExtendedData;
    }

    public void setSpringControllerMethodHashSet(Set<String> springControllerMethodHashSet) {
        this.springControllerMethodHashSet = springControllerMethodHashSet;
    }

    public void setWithAnnotationMethodHashSet(Set<String> withAnnotationMethodHashSet) {
        this.withAnnotationMethodHashSet = withAnnotationMethodHashSet;
    }

    public void setWithInfoCallIdSet(Set<Integer> withInfoCallIdSet) {
        this.withInfoCallIdSet = withInfoCallIdSet;
    }

    public void setMethodCallAddExtList(List<MethodCallAddInterface> methodCallAddExtList) {
        this.methodCallAddExtList = methodCallAddExtList;
    }

    public void setExtendedDataAddExtList(List<ExtendedDataAddInterface> extendedDataAddExtList) {
        this.extendedDataAddExtList = extendedDataAddExtList;
    }
}
