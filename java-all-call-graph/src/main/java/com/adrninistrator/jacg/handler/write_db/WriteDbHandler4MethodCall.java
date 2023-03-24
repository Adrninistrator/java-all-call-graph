package com.adrninistrator.jacg.handler.write_db;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.MethodCallFlagsEnum;
import com.adrninistrator.jacg.dto.method.MethodCallFullMethod;
import com.adrninistrator.jacg.dto.write_db.WriteDbData4MethodCall;
import com.adrninistrator.jacg.extensions.method_call_add.MethodCallAddInterface;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.common.enums.JavaCGCallTypeEnum;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2022/11/18
 * @description: 写入数据库，方法调用关系
 */
public class WriteDbHandler4MethodCall extends AbstractWriteDbHandler<WriteDbData4MethodCall> {
    private static final Logger logger = LoggerFactory.getLogger(WriteDbHandler4MethodCall.class);

    // Spring Controller对应的方法HASH+长度
    private Set<String> springControllerMethodHashSet;

    // 有注解的方法HASH+长度
    private Set<String> withAnnotationMethodHashSet;

    // 被调用对象及参数存在信息的call_id
    private Set<Integer> withInfoCallIdSet;

    // 方法参数存在泛型类型的方法HASH+长度
    private Set<String> withGenericsTypeMethodHash;

    // 人工添加方法调用关系类列表
    private List<MethodCallAddInterface> methodCallAddExtList;

    // 保存MyBatis Mapper类名
    private Set<String> myBatisMapperSet;

    // 保存MyBatis写数据库的Mapper方法
    private Set<String> myBatisMapperMethodWriteSet;

    // 人工添加的方法调用关系
    private final List<MethodCallFullMethod> manualAddMethodCallList = new ArrayList<>(batchSize);

    @Override
    protected WriteDbData4MethodCall genData(String line) {
        String[] array = splitEquals(line, 8);

        int callId = Integer.parseInt(array[0]);
        String callerFullMethod = array[1];
        String tmpCalleeFullMethod = array[2];
        int callerLineNum = Integer.parseInt(array[3]);
        String calleeObjType = array[4];
        String rawReturnType = array[5];
        String actualReturnType = array[6];
        String callerJarNum = array[7];

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
                callerJarNum,
                rawReturnType,
                actualReturnType
        );

        if (writeDbData4MethodCall.getCallerMethodHash().equals(writeDbData4MethodCall.getCalleeMethodHash())) {
            // 对于递归调用，不写入数据库，防止查询时出现死循环
            logger.debug("递归调用不写入数据库 {}", line);
            return null;
        }

        // 生成方法调用标记
        genCallFlags(callId, writeDbData4MethodCall);

        // 人工添加方法调用关系
        for (MethodCallAddInterface methodCallAddExt : methodCallAddExtList) {
            MethodCallFullMethod methodCallFullMethod = methodCallAddExt.handleMethodCall(callerFullMethod, calleeFullMethod, calleeClassName);
            if (methodCallFullMethod != null) {
                manualAddMethodCallList.add(methodCallFullMethod);
            }
        }

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

    /**
     * 生成方法调用标记
     *
     * @param callId
     * @param writeDbData4MethodCall
     * @return
     */
    private void genCallFlags(int callId, WriteDbData4MethodCall writeDbData4MethodCall) {
        String callerMethodHash = writeDbData4MethodCall.getCallerMethodHash();
        String calleeMethodHash = writeDbData4MethodCall.getCalleeMethodHash();
        String calleeClassName = JACGClassMethodUtil.getClassNameFromMethod(writeDbData4MethodCall.getCalleeFullMethod());
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
        if (withGenericsTypeMethodHash.contains(calleeMethodHash)) {
            callFlags = MethodCallFlagsEnum.MCFE_EE_WITH_GENERICS_TYPE.setFlag(callFlags);
        }
        if (withGenericsTypeMethodHash.contains(callerMethodHash)) {
            callFlags = MethodCallFlagsEnum.MCFE_ER_WITH_GENERICS_TYPE.setFlag(callFlags);
        }
        if (myBatisMapperSet.contains(calleeClassName)) {
            callFlags = MethodCallFlagsEnum.MCFE_EE_MYBATIS_MAPPER.setFlag(callFlags);
            String calleeMethodName = JACGClassMethodUtil.getMethodNameFromFull(writeDbData4MethodCall.getCalleeFullMethod());
            String calleeClassAndMethodName = JACGClassMethodUtil.getClassAndMethodName(calleeClassName, calleeMethodName);
            if (myBatisMapperMethodWriteSet.contains(calleeClassAndMethodName)) {
                callFlags = MethodCallFlagsEnum.MCFE_EE_MYBATIS_MAPPER_WRITE.setFlag(callFlags);
            }
        }
        writeDbData4MethodCall.setCallFlags(callFlags);
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
                    String.valueOf(0),
                    "",
                    ""
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
    public void setSpringControllerMethodHashSet(Set<String> springControllerMethodHashSet) {
        this.springControllerMethodHashSet = springControllerMethodHashSet;
    }

    public void setWithAnnotationMethodHashSet(Set<String> withAnnotationMethodHashSet) {
        this.withAnnotationMethodHashSet = withAnnotationMethodHashSet;
    }

    public void setWithInfoCallIdSet(Set<Integer> withInfoCallIdSet) {
        this.withInfoCallIdSet = withInfoCallIdSet;
    }

    public void setWithGenericsTypeMethodHash(Set<String> withGenericsTypeMethodHash) {
        this.withGenericsTypeMethodHash = withGenericsTypeMethodHash;
    }

    public void setMethodCallAddExtList(List<MethodCallAddInterface> methodCallAddExtList) {
        this.methodCallAddExtList = methodCallAddExtList;
    }

    public void setMyBatisMapperSet(Set<String> myBatisMapperSet) {
        this.myBatisMapperSet = myBatisMapperSet;
    }

    public void setMyBatisMapperMethodWriteSet(Set<String> myBatisMapperMethodWriteSet) {
        this.myBatisMapperMethodWriteSet = myBatisMapperMethodWriteSet;
    }
}
