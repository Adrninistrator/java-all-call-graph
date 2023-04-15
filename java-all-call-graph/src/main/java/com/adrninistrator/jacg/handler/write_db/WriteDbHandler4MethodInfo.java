package com.adrninistrator.jacg.handler.write_db;

import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.write_db.WriteDbData4MethodArgType;
import com.adrninistrator.jacg.dto.write_db.WriteDbData4MethodInfo;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGUtil;

import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2022/11/16
 * @description: 写入数据库，方法的信息
 */
public class WriteDbHandler4MethodInfo extends AbstractWriteDbHandler<WriteDbData4MethodInfo> {

    // 方法的参数类型写入数据库的类
    private WriteDbHandler4MethodArgType writeDbHandler4MethodArgType;

    // 方法的参数类型相关信息
    private final List<WriteDbData4MethodArgType> writeDbData4MethodArgTypeList = new ArrayList<>(batchSize);

    @Override
    protected WriteDbData4MethodInfo genData(String line) {
        String[] array = splitEquals(line, 3);

        String fullMethod = array[0];
        // 根据完整方法前缀判断是否需要处理
        if (!isAllowedClassPrefix(fullMethod)) {
            return null;
        }

        String methodHash = JACGUtil.genHashWithLen(fullMethod);
        String accessFlags = array[1];
        String className = JACGClassMethodUtil.getClassNameFromMethod(fullMethod);
        String simpleClassName = dbOperWrapper.getSimpleClassName(className);
        String methodName = JACGClassMethodUtil.getMethodNameFromFull(fullMethod);
        String returnType = array[2];
        String simpleReturnType = dbOperWrapper.getSimpleClassName(returnType);

        // 处理方法的参数类型
        handleMethodArgType(fullMethod, methodHash, simpleClassName);

        return new WriteDbData4MethodInfo(methodHash,
                simpleClassName,
                Integer.parseInt(accessFlags),
                methodName,
                fullMethod,
                simpleReturnType,
                returnType);
    }

    @Override
    protected DbTableInfoEnum chooseDbTableInfo() {
        return DbTableInfoEnum.DTIE_METHOD_INFO;
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
                data.getReturnType()
        };
    }

    @Override
    protected void beforeDone() {
        // 写入方法的参数类型剩余信息
        writeDbHandler4MethodArgType.insertDb(writeDbData4MethodArgTypeList);
    }

    // 处理方法的参数类型
    private void handleMethodArgType(String fullMethod, String methodHash, String simpleClassName) {
        List<String> argTypeList = JACGClassMethodUtil.genMethodArgTypeList(fullMethod);
        if (argTypeList.isEmpty()) {
            // 方法无参数
            return;
        }
        for (int i = 0; i < argTypeList.size(); i++) {
            String argType = argTypeList.get(i);
            String simpleArgType = dbOperWrapper.getSimpleClassName(argType);
            WriteDbData4MethodArgType writeDbData4MethodArgType = new WriteDbData4MethodArgType(methodHash, i, simpleArgType, argType, simpleClassName, fullMethod);
            writeDbData4MethodArgTypeList.add(writeDbData4MethodArgType);
            // 尝试写入方法的参数类型
            writeDbHandler4MethodArgType.tryInsertDb(writeDbData4MethodArgTypeList);
        }
    }

    public void setWriteDbHandler4MethodArgType(WriteDbHandler4MethodArgType writeDbHandler4MethodArgType) {
        this.writeDbHandler4MethodArgType = writeDbHandler4MethodArgType;
    }
}
