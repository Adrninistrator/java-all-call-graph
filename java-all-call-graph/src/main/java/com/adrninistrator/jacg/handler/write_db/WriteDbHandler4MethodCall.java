package com.adrninistrator.jacg.handler.write_db;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.MethodCallFlagsEnum;
import com.adrninistrator.jacg.dto.write_db.WriteDbData4MethodCall;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.common.enums.JavaCGOutPutFileTypeEnum;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Set;

/**
 * @author adrninistrator
 * @date 2022/11/18
 * @description: 写入数据库，方法调用关系
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCGOutPutFileTypeEnum.OPFTE_METHOD_CALL,
        minColumnNum = 9,
        maxColumnNum = 9,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_METHOD_CALL
)
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

    // 保存MyBatis Mapper类名
    private Set<String> myBatisMapperSet;

    // 保存MyBatis写数据库的Mapper方法
    private Set<String> myBatisMapperMethodWriteSet;

    @Override
    protected WriteDbData4MethodCall genData(String[] array) {
        int callId = Integer.parseInt(array[0]);
        String callerFullMethod = array[1];
        String tmpCalleeFullMethod = array[2];

        int indexCalleeLeftBracket = tmpCalleeFullMethod.indexOf(JavaCGConstants.FILE_KEY_CALL_TYPE_FLAG1);
        int indexCalleeRightBracket = tmpCalleeFullMethod.indexOf(JavaCGConstants.FILE_KEY_CALL_TYPE_FLAG2);
        String calleeFullMethod = tmpCalleeFullMethod.substring(indexCalleeRightBracket + JavaCGConstants.FILE_KEY_CALL_TYPE_FLAG2.length()).trim();

        // 根据完整方法前缀判断是否需要处理
        if (!isAllowedClassPrefix(callerFullMethod) && !isAllowedClassPrefix(calleeFullMethod)) {
            return null;
        }

        int callerLineNum = Integer.parseInt(array[3]);
        String calleeObjType = array[4];
        String rawReturnType = array[5];
        String actualReturnType = array[6];
        String callerJarNumStr = array[7];
        String calleeJarNumStr = array[8];

        String callType = tmpCalleeFullMethod.substring(indexCalleeLeftBracket + JavaCGConstants.FILE_KEY_CALL_TYPE_FLAG1.length(), indexCalleeRightBracket);
        String callerClassName = JACGClassMethodUtil.getClassNameFromMethod(callerFullMethod);
        String calleeClassName = JACGClassMethodUtil.getClassNameFromMethod(calleeFullMethod);
        Integer callerJarNum = (JavaCGConstants.EMPTY_JAR_NUM.equals(callerJarNumStr) ? null : Integer.parseInt(callerJarNumStr));
        Integer calleeJarNum = (JavaCGConstants.EMPTY_JAR_NUM.equals(calleeJarNumStr) ? null : Integer.parseInt(calleeJarNumStr));

        WriteDbData4MethodCall writeDbData4MethodCall = WriteDbData4MethodCall.genInstance(
                callType,
                calleeObjType,
                dbOperWrapper.getSimpleClassName(callerClassName),
                callerFullMethod,
                dbOperWrapper.getSimpleClassName(calleeClassName),
                calleeFullMethod,
                callId,
                callerLineNum,
                rawReturnType,
                actualReturnType,
                callerJarNum,
                calleeJarNum
        );

        if (writeDbData4MethodCall.getCallerMethodHash().equals(writeDbData4MethodCall.getCalleeMethodHash())) {
            // 对于递归调用，不写入数据库，防止查询时出现死循环
            if (logger.isDebugEnabled()) {
                logger.debug("递归调用不写入数据库 {}", StringUtils.join(array, "\t"));
            }
            return null;
        }

        // 生成方法调用标记
        genCallFlags(callId, writeDbData4MethodCall);
        return writeDbData4MethodCall;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4MethodCall data) {
        return JACGUtil.genMethodCallObjectArray(data);
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "方法调用序号",
                "调用方，完整方法（类名+方法名+参数）",
                "被调用方，完整方法（类名+方法名+参数）",
                "调用方，源代码行号",
                "被调用对象类型，t:调用当前实例的方法，sf:调用静态字段的方法，f:调用字段的方法，v:调用其他变量的方法",
                "方法原始的返回类型",
                "方法实际的返回类型",
                "调用方，Jar包序号",
                "被调用方，Jar包序号"
        };
    }


    @Override
    public String[] chooseOtherFileDetailInfo() {
        return new String[]{
                "方法调用信息，每个方法调用占一行，包括调用者方法与被调用者方法"
        };
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

    public void setMyBatisMapperSet(Set<String> myBatisMapperSet) {
        this.myBatisMapperSet = myBatisMapperSet;
    }

    public void setMyBatisMapperMethodWriteSet(Set<String> myBatisMapperMethodWriteSet) {
        this.myBatisMapperMethodWriteSet = myBatisMapperMethodWriteSet;
    }
}
