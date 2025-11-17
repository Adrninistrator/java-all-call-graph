package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.MethodCallFlagsEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCall;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.extensions.methodcall.AbstractJACGMethodCallExtension;
import com.adrninistrator.jacg.handler.method.MethodInfoHandler;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.common.enums.JavaCG2OutPutFileTypeEnum;
import com.adrninistrator.javacg2.common.enums.JavaCG2YesNoEnum;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;
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
        mainFileTypeEnum = JavaCG2OutPutFileTypeEnum.OPFTE_METHOD_CALL,
        minColumnNum = 13,
        maxColumnNum = 13,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_METHOD_CALL,
        dependsWriteDbTableEnums = {
                DbTableInfoEnum.DTIE_CLASS_NAME,
                DbTableInfoEnum.DTIE_METHOD_ANNOTATION,
                DbTableInfoEnum.DTIE_METHOD_INFO,
                DbTableInfoEnum.DTIE_METHOD_ARG_GENERICS_TYPE,
                DbTableInfoEnum.DTIE_METHOD_RETURN_GENERICS_TYPE,
                DbTableInfoEnum.DTIE_EXTENDS_IMPL,
                DbTableInfoEnum.DTIE_METHOD_CALL_INFO,
                DbTableInfoEnum.DTIE_MYBATIS_MS_TABLE,
                DbTableInfoEnum.DTIE_MYBATIS_MS_WRITE_TABLE,
                DbTableInfoEnum.DTIE_GET_METHOD,
                DbTableInfoEnum.DTIE_SET_METHOD}
)
public class WriteDbHandler4MethodCall extends AbstractWriteDbHandler<WriteDbData4MethodCall> {

    private static final Logger logger = LoggerFactory.getLogger(WriteDbHandler4MethodCall.class);
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
            get方法名Set
     */
    private Map<String, Set<String>> getMethodSimpleClassMap;


    /*
        set方法对应的信息
        key
            唯一类名
        value
            set方法名Set
     */
    private Map<String, Set<String>> setMethodSimpleClassMap;

    // 是否使用方法调用处理扩展类
    private boolean useJACGMethodCallExtension;

    // 方法调用处理扩展类
    private List<AbstractJACGMethodCallExtension> jacgMethodCallExtensionList;

    private MethodInfoHandler methodInfoHandler;

    public WriteDbHandler4MethodCall(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    public void init(WriteDbResult writeDbResult) {
        super.init(writeDbResult);
        methodInfoHandler = new MethodInfoHandler(dbOperWrapper);
    }

    @Override
    protected WriteDbData4MethodCall genData(String[] array) {
        int callId = Integer.parseInt(readLineData());
        boolean enabled = JavaCG2YesNoEnum.isYes(readLineData());
        String callerFullMethod = readLineData();
        String tmpCalleeFullMethod = readLineData();

        int indexCalleeRightBracket = tmpCalleeFullMethod.indexOf(JavaCG2Constants.FILE_KEY_CALL_TYPE_FLAG2);
        String calleeFullMethod = tmpCalleeFullMethod.substring(indexCalleeRightBracket + JavaCG2Constants.FILE_KEY_CALL_TYPE_FLAG2.length()).trim();
        int callerLineNum = Integer.parseInt(readLineData());
        String callerReturnType = readLineData();
        int calleeArrayDimensions = Integer.parseInt(readLineData());
        String calleeObjType = readLineData();
        String rawReturnType = readLineData();
        String actualReturnType = readLineData();
        String callerJarNumStr = readLineData();
        String calleeJarNumStr = readLineData();
        String description = readLineData();

        int indexCalleeLeftBracket = tmpCalleeFullMethod.indexOf(JavaCG2Constants.FILE_KEY_CALL_TYPE_FLAG1);
        String callType = tmpCalleeFullMethod.substring(indexCalleeLeftBracket + JavaCG2Constants.FILE_KEY_CALL_TYPE_FLAG1.length(), indexCalleeRightBracket);
        String callerClassName = JavaCG2ClassMethodUtil.getClassNameFromMethod(callerFullMethod);
        String calleeClassName = JavaCG2ClassMethodUtil.getClassNameFromMethod(calleeFullMethod);
        Integer callerJarNum = JACGUtil.parseJarNum(callerJarNumStr);
        Integer calleeJarNum = JACGUtil.parseJarNum(calleeJarNumStr);

        if (JACGConstants.RETURN_TYPE_FLAG_PLACE_HOLDER.equals(rawReturnType) || JACGConstants.RETURN_TYPE_FLAG_PLACE_HOLDER.equals(actualReturnType)) {
            // 被调用方法返回类型使用占位符，需要查询被调用方法的参数类型与返回类型
            String calleeMethodName = JavaCG2ClassMethodUtil.getMethodNameFromFull(calleeFullMethod);
            List<WriteDbData4MethodInfo> methodInfoList = methodInfoHandler.queryMethodInfoByClassMethodSuperInterface(calleeClassName, calleeMethodName);
            if (JavaCG2Util.isCollectionEmpty(methodInfoList)) {
                logger.error("获取被调用方法返回类型，根据类名与方法名查询到的方法预期为1个，实际为空 {} {} {} {}", callerFullMethod, callerLineNum, calleeClassName, calleeMethodName);
                return null;
            }
            if (methodInfoList.size() != 1) {
                logger.error("获取被调用方法返回类型，根据类名与方法名查询到的方法预期为1个，实际存在多个 {} {} {} {} {}", callerFullMethod, callerLineNum, calleeClassName, calleeMethodName, methodInfoList.size());
                return null;
            }
            WriteDbData4MethodInfo methodInfo = methodInfoList.get(0);
            logger.info("修改根据类名与方法名查询到的完整方法及返回类型 {} {} {} {}", calleeClassName, calleeMethodName, methodInfo.getFullMethod(), methodInfo.getReturnType());
            calleeFullMethod = methodInfo.getFullMethod();
            rawReturnType = methodInfo.getReturnType();
            actualReturnType = methodInfo.getReturnType();
        }

        WriteDbData4MethodCall writeDbData4MethodCall = WriteDbData4MethodCall.genInstance(
                callId,
                enabled,
                callType,
                dbOperWrapper.querySimpleClassName(callerClassName),
                callerFullMethod,
                callerLineNum,
                callerReturnType,
                dbOperWrapper.querySimpleClassName(calleeClassName),
                calleeFullMethod,
                calleeArrayDimensions,
                calleeObjType,
                rawReturnType,
                actualReturnType,
                callerJarNum,
                calleeJarNum,
                description
        );

        // 对于递归调用，写入数据库，查询时有对死循环进行处理

        int modifyTimes = 0;
        // 使用扩展类对方法调用进行处理
        if (useJACGMethodCallExtension) {
            for (AbstractJACGMethodCallExtension jacgMethodCallExtension : jacgMethodCallExtensionList) {
                if (jacgMethodCallExtension.handle(writeDbData4MethodCall)) {
                    modifyTimes++;
                }
            }
        }

        if (modifyTimes == 0) {
            // 生成方法调用标志
            genCallFlags(callId, writeDbData4MethodCall);
        }
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
                "是否启用，1:启用，0:未启用",
                "调用方，完整方法（类名+方法名+参数）",
                "(方法调用类型)被调用方，完整方法（类名+方法名+参数）",
                "调用方法源代码行号",
                "调用方法返回类型",
                "被调用方，对象数组的维度，为0代表不是数组类型",
                "被调用对象类型，t:调用当前实例的方法，sf:调用静态字段的方法，f:调用字段的方法，v:调用其他变量的方法",
                "被调用方法原始的返回类型",
                "被调用方法实际的返回类型",
                "调用方法jar文件序号",
                "被调用方法jar文件序号",
                "描述信息，默认为空"
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
        String calleeClassName = JavaCG2ClassMethodUtil.getClassNameFromMethod(writeDbData4MethodCall.getCalleeFullMethod());
        String calleeSimpleClassName = dbOperWrapper.querySimpleClassName(calleeClassName);
        String calleeMethodName = JavaCG2ClassMethodUtil.getMethodNameFromFull(writeDbData4MethodCall.getCalleeFullMethod());
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

    public void recordJacgMethodCallExtensionList(List<AbstractJACGMethodCallExtension> jacgMethodCallExtensionList) {
        this.jacgMethodCallExtensionList = jacgMethodCallExtensionList;
        useJACGMethodCallExtension = !JavaCG2Util.isCollectionEmpty(jacgMethodCallExtensionList);
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
