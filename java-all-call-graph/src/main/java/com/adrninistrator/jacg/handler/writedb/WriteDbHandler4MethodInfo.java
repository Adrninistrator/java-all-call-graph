package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.javacg2.common.enums.JavaCG2OutPutFileTypeEnum;
import com.adrninistrator.javacg2.util.JavaCG2ByteCodeUtil;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;

/**
 * @author adrninistrator
 * @date 2022/11/16
 * @description: 写入数据库，方法的信息
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCG2OutPutFileTypeEnum.OPFTE_METHOD_INFO,
        minColumnNum = 8,
        maxColumnNum = 8,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_METHOD_INFO
)
public class WriteDbHandler4MethodInfo extends AbstractWriteDbHandler<WriteDbData4MethodInfo> {

    public WriteDbHandler4MethodInfo(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4MethodInfo genData(String[] array) {
        String fullMethod = readLineData();
        String accessFlags = readLineData();
        String className = JavaCG2ClassMethodUtil.getClassNameFromMethod(fullMethod);
        String simpleClassName = dbOperWrapper.querySimpleClassName(className);
        String methodName = JavaCG2ClassMethodUtil.getMethodNameFromFull(fullMethod);
        String returnTypeNad = readLineData();
        int returnArrayDimensions = Integer.parseInt(readLineData());
        String returnType = JavaCG2ByteCodeUtil.addArrayFlag(returnTypeNad, returnArrayDimensions);
        String methodHash = JACGClassMethodUtil.genMethodHashWithLen(fullMethod, returnType);
        String returnCategory = readLineData();
        int returnExistsGenericsType = Integer.parseInt(readLineData());
        String simpleReturnTypeNad = dbOperWrapper.querySimpleClassName(returnTypeNad);
        String methodInstructionsHash = readLineData();
        int jarNum = Integer.parseInt(readLineData());

        WriteDbData4MethodInfo methodInfo = new WriteDbData4MethodInfo();
        methodInfo.setRecordId(genNextRecordId());
        methodInfo.setMethodHash(methodHash);
        methodInfo.setSimpleClassName(simpleClassName);
        methodInfo.setAccessFlags(Integer.parseInt(accessFlags));
        methodInfo.setMethodName(methodName);
        methodInfo.setSimpleReturnTypeNad(simpleReturnTypeNad);
        methodInfo.setReturnTypeNad(returnTypeNad);
        methodInfo.setReturnArrayDimensions(returnArrayDimensions);
        methodInfo.setReturnType(returnType);
        methodInfo.setReturnCategory(returnCategory);
        methodInfo.setReturnExistsGenericsType(returnExistsGenericsType);
        methodInfo.setClassName(className);
        methodInfo.setFullMethod(fullMethod);
        methodInfo.setMethodInstructionsHash(methodInstructionsHash);
        methodInfo.setJarNum(jarNum);
        return methodInfo;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4MethodInfo data) {
        return new Object[]{
                data.getRecordId(),
                data.getMethodHash(),
                data.getSimpleClassName(),
                data.getAccessFlags(),
                data.getMethodName(),
                data.getSimpleReturnTypeNad(),
                data.getReturnTypeNad(),
                data.getReturnArrayDimensions(),
                data.getReturnType(),
                data.getReturnCategory(),
                data.getReturnExistsGenericsType(),
                data.getClassName(),
                data.getFullMethod(),
                data.getMethodInstructionsHash(),
                data.getJarNum()
        };
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "完整方法（类名+方法名+参数）",
                "方法的access_flags",
                "返回类型类名（不包含数组标志）",
                "返回类型数组的维度，为0代表不是数组类型",
                "返回类型分类，J:JDK中的类型，C:自定义类型",
                "返回类型是否存在泛型类型，1:是，0:否",
                "方法指令的HASH值（MD5），可能为空字符串",
                "方法所在的jar文件序号"
        };
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "方法的信息，包括完整方法、access_flags、返回类型、方法指令的HASH值（MD5）等"
        };
    }
}
