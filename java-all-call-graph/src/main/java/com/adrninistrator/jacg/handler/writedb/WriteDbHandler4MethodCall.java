package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.MethodCallFlagsEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCall;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.common.enums.JavaCGOutPutFileTypeEnum;

import java.util.Map;
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
        minColumnNum = 10,
        maxColumnNum = 10,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_METHOD_CALL
)
public class WriteDbHandler4MethodCall extends AbstractWriteDbHandler<WriteDbData4MethodCall> {

    /*
        涉及继承的唯一类名
        key     子类唯一类名
        value   对应的父类唯一类名
     */
    private Map<String, String> extendsSimpleClassNameMap;

    // 有注解的方法HASH+长度
    private Set<String> withAnnotationMethodHashSet;

    // 被调用对象及参数存在信息的call_id
    private Set<Integer> withInfoCallIdSet;

    // 方法参数存在泛型类型的方法HASH+长度
    private Set<String> withArgsGenericsTypeMethodHashSet;

    // 方法返回存在泛型类型的方法HASH+长度
    private Set<String> withReturnGenericsTypeMethodHashSet;

    // 保存MyBatis Mapper方法
    private Set<String> myBatisMapperMethodSet;

    // 保存MyBatis写数据库的Mapper方法
    private Set<String> myBatisMapperMethodWriteSet;

    /*
        get方法对应的信息
        key
            唯一类名
        value
            get方法名称Set
     */
    private Map<String, Set<String>> getMethodSimpleClassMap;


    /*
        set方法对应的信息
        key
            唯一类名
        value
            set方法名称Set
     */
    private Map<String, Set<String>> setMethodSimpleClassMap;

    public WriteDbHandler4MethodCall(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4MethodCall genData(String[] array) {
        int callId = Integer.parseInt(array[0]);
        String callerFullMethod = array[1];
        String tmpCalleeFullMethod = array[2];

        int indexCalleeRightBracket = tmpCalleeFullMethod.indexOf(JavaCGConstants.FILE_KEY_CALL_TYPE_FLAG2);
        String calleeFullMethod = tmpCalleeFullMethod.substring(indexCalleeRightBracket + JavaCGConstants.FILE_KEY_CALL_TYPE_FLAG2.length()).trim();

        // 根据完整方法前缀判断是否需要处理
        if (!isAllowedClassPrefix(callerFullMethod) && !isAllowedClassPrefix(calleeFullMethod)) {
            return null;
        }

        int callerLineNum = Integer.parseInt(array[3]);
        String callerReturnType = array[4];
        String calleeObjType = array[5];
        String rawReturnType = array[6];
        String actualReturnType = array[7];
        String callerJarNumStr = array[8];
        String calleeJarNumStr = array[9];

        int indexCalleeLeftBracket = tmpCalleeFullMethod.indexOf(JavaCGConstants.FILE_KEY_CALL_TYPE_FLAG1);
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
                callerReturnType,
                rawReturnType,
                actualReturnType,
                callerJarNum,
                calleeJarNum
        );

        // 对于递归调用，写入数据库，查询时有对死循环进行处理
        // 生成方法调用标志
        genCallFlags(callId, writeDbData4MethodCall);
        return writeDbData4MethodCall;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4MethodCall data) {
        return JACGSqlUtil.genMethodCallObjectArray(data);
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "方法调用序号，从1开始",
                "调用方，完整方法（类名+方法名+参数）",
                "(方法调用类型)被调用方，完整方法（类名+方法名+参数）",
                "调用方法源代码行号",
                "调用方法返回类型",
                "被调用对象类型，t:调用当前实例的方法，sf:调用静态字段的方法，f:调用字段的方法，v:调用其他变量的方法",
                "被调用方法原始的返回类型",
                "被调用方法实际的返回类型",
                "调用方法Jar包序号",
                "被调用方法Jar包序号"
        };
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "方法调用信息，每个方法调用占一行，包括调用方方法与被调用方方法",
                "方法调用类型，详细信息见[call_type.md](call_type.md)"
        };
    }

    /**
     * 生成方法调用标志
     *
     * @param callId
     * @param writeDbData4MethodCall
     * @return
     */
    private void genCallFlags(int callId, WriteDbData4MethodCall writeDbData4MethodCall) {
        String callerMethodHash = writeDbData4MethodCall.getCallerMethodHash();
        String calleeMethodHash = writeDbData4MethodCall.getCalleeMethodHash();
        String calleeClassName = JACGClassMethodUtil.getClassNameFromMethod(writeDbData4MethodCall.getCalleeFullMethod());
        String calleeSimpleClassName = dbOperWrapper.getSimpleClassName(calleeClassName);
        String calleeMethodName = JACGClassMethodUtil.getMethodNameFromFull(writeDbData4MethodCall.getCalleeFullMethod());
        int callFlags = 0;

        if (withAnnotationMethodHashSet.contains(callerMethodHash)) {
            callFlags = MethodCallFlagsEnum.MCFE_ER_METHOD_ANNOTATION.setFlag(callFlags);
        }

        if (withAnnotationMethodHashSet.contains(calleeMethodHash)) {
            callFlags = MethodCallFlagsEnum.MCFE_EE_METHOD_ANNOTATION.setFlag(callFlags);
        }

        if (withInfoCallIdSet.contains(callId)) {
            callFlags = MethodCallFlagsEnum.MCFE_METHOD_CALL_INFO.setFlag(callFlags);
        }

        if (withArgsGenericsTypeMethodHashSet.contains(calleeMethodHash)) {
            callFlags = MethodCallFlagsEnum.MCFE_EE_ARGS_WITH_GENERICS_TYPE.setFlag(callFlags);
        }

        if (withArgsGenericsTypeMethodHashSet.contains(callerMethodHash)) {
            callFlags = MethodCallFlagsEnum.MCFE_ER_ARGS_WITH_GENERICS_TYPE.setFlag(callFlags);
        }

        if (withReturnGenericsTypeMethodHashSet.contains(calleeMethodHash)) {
            callFlags = MethodCallFlagsEnum.MCFE_EE_RETURN_WITH_GENERICS_TYPE.setFlag(callFlags);
        }

        if (withReturnGenericsTypeMethodHashSet.contains(callerMethodHash)) {
            callFlags = MethodCallFlagsEnum.MCFE_ER_RETURN_WITH_GENERICS_TYPE.setFlag(callFlags);
        }

        String calleeClassAndMethodName = JACGClassMethodUtil.genClassAndMethodName(calleeClassName, calleeMethodName);
        if (myBatisMapperMethodSet.contains(calleeClassAndMethodName)) {
            callFlags = MethodCallFlagsEnum.MCFE_EE_MYBATIS_MAPPER.setFlag(callFlags);
            if (myBatisMapperMethodWriteSet.contains(calleeClassAndMethodName)) {
                callFlags = MethodCallFlagsEnum.MCFE_EE_MYBATIS_MAPPER_WRITE.setFlag(callFlags);
            }
        }

        // 检查是否为dto的get/set方法
        if (checkDtoGetSetMethod(true, calleeSimpleClassName, calleeMethodName, getMethodSimpleClassMap, extendsSimpleClassNameMap)) {
            callFlags = MethodCallFlagsEnum.MCFE_EE_DTO_GET_SET_METHOD.setFlag(callFlags);
        } else if (checkDtoGetSetMethod(false, calleeSimpleClassName, calleeMethodName, setMethodSimpleClassMap, extendsSimpleClassNameMap)) {
            callFlags = MethodCallFlagsEnum.MCFE_EE_DTO_GET_SET_METHOD.setFlag(callFlags);
        }
        writeDbData4MethodCall.setCallFlags(callFlags);
    }

    //
    public void setExtendsSimpleClassNameMap(Map<String, String> extendsSimpleClassNameMap) {
        this.extendsSimpleClassNameMap = extendsSimpleClassNameMap;
    }

    public void setWithAnnotationMethodHashSet(Set<String> withAnnotationMethodHashSet) {
        this.withAnnotationMethodHashSet = withAnnotationMethodHashSet;
    }

    public void setWithInfoCallIdSet(Set<Integer> withInfoCallIdSet) {
        this.withInfoCallIdSet = withInfoCallIdSet;
    }

    public void setWithArgsGenericsTypeMethodHashSet(Set<String> withArgsGenericsTypeMethodHashSet) {
        this.withArgsGenericsTypeMethodHashSet = withArgsGenericsTypeMethodHashSet;
    }

    public void setWithReturnGenericsTypeMethodHashSet(Set<String> withReturnGenericsTypeMethodHashSet) {
        this.withReturnGenericsTypeMethodHashSet = withReturnGenericsTypeMethodHashSet;
    }

    public void setMyBatisMapperMethodSet(Set<String> myBatisMapperMethodSet) {
        this.myBatisMapperMethodSet = myBatisMapperMethodSet;
    }

    public void setMyBatisMapperMethodWriteSet(Set<String> myBatisMapperMethodWriteSet) {
        this.myBatisMapperMethodWriteSet = myBatisMapperMethodWriteSet;
    }

    public Map<String, Set<String>> getGetMethodSimpleClassMap() {
        return getMethodSimpleClassMap;
    }

    public void setGetMethodSimpleClassMap(Map<String, Set<String>> getMethodSimpleClassMap) {
        this.getMethodSimpleClassMap = getMethodSimpleClassMap;
    }

    public Map<String, Set<String>> getSetMethodSimpleClassMap() {
        return setMethodSimpleClassMap;
    }

    public void setSetMethodSimpleClassMap(Map<String, Set<String>> setMethodSimpleClassMap) {
        this.setMethodSimpleClassMap = setMethodSimpleClassMap;
    }
}
