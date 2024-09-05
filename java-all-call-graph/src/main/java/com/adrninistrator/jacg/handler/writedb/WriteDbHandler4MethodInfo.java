package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg2.common.enums.JavaCG2OutPutFileTypeEnum;

/**
 * @author adrninistrator
 * @date 2022/11/16
 * @description: 写入数据库，方法的信息
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCG2OutPutFileTypeEnum.OPFTE_METHOD_INFO,
        minColumnNum = 5,
        maxColumnNum = 5,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_METHOD_INFO
)
public class WriteDbHandler4MethodInfo extends AbstractWriteDbHandler<WriteDbData4MethodInfo> {

    public WriteDbHandler4MethodInfo(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4MethodInfo genData(String[] array) {
        String fullMethod = array[0];
        // 根据完整方法前缀判断是否需要处理
        if (!isAllowedClassPrefix(fullMethod)) {
            return null;
        }

        String methodHash = JACGUtil.genHashWithLen(fullMethod);
        String accessFlags = array[1];
        String className = JACGClassMethodUtil.getClassNameFromMethod(fullMethod);
        String simpleClassName = dbOperWrapper.querySimpleClassName(className);
        String methodName = JACGClassMethodUtil.getMethodNameFromFull(fullMethod);
        String returnType = array[2];
        String simpleReturnType = dbOperWrapper.querySimpleClassName(returnType);
        String methodInstructionsHash = array[3];
        int jarNum = Integer.parseInt(array[4]);

        WriteDbData4MethodInfo methodInfo = new WriteDbData4MethodInfo();
        methodInfo.setMethodHash(methodHash);
        methodInfo.setSimpleClassName(simpleClassName);
        methodInfo.setAccessFlags(Integer.parseInt(accessFlags));
        methodInfo.setMethodName(methodName);
        methodInfo.setFullMethod(fullMethod);
        methodInfo.setSimpleReturnType(simpleReturnType);
        methodInfo.setReturnType(returnType);
        methodInfo.setMethodInstructionsHash(methodInstructionsHash);
        methodInfo.setJarNum(jarNum);
        methodInfo.setClassName(className);
        return methodInfo;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4MethodInfo data) {
        return new Object[]{
                data.getMethodHash(),
                data.getSimpleClassName(),
                data.getAccessFlags(),
                data.getMethodName(),
                data.getFullMethod(),
                data.getSimpleReturnType(),
                data.getReturnType(),
                data.getMethodInstructionsHash(),
                data.getJarNum(),
                data.getClassName()
        };
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "完整方法（类名+方法名+参数）",
                "方法的access_flags",
                "返回类型类名",
                "方法指令的HASH值（MD5），可能为空字符串",
                "方法所在的Jar包序号"
        };
    }


    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "方法的信息，包括完整方法、access_flags、返回类型、方法指令的HASH值（MD5）等"
        };
    }
}
