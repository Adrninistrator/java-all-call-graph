package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCatch;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg.common.enums.JavaCGOutPutFileTypeEnum;

/**
 * @author adrninistrator
 * @date 2023/12/2
 * @description: 写入数据库，方法的catch信息
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCGOutPutFileTypeEnum.OPFTE_METHOD_CATCH,
        minColumnNum = 13,
        maxColumnNum = 13,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_METHOD_CATCH
)
public class WriteDbHandler4MethodCatch extends AbstractWriteDbHandler<WriteDbData4MethodCatch> {

    public WriteDbHandler4MethodCatch(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4MethodCatch genData(String[] array) {
        String fullMethod = array[0];
        // 根据完整方法前缀判断是否需要处理
        if (!isAllowedClassPrefix(fullMethod)) {
            return null;
        }

        String className = JACGClassMethodUtil.getClassNameFromMethod(fullMethod);
        String methodName = JACGClassMethodUtil.getMethodNameFromFull(fullMethod);
        String methodHash = JACGUtil.genHashWithLen(fullMethod);
        String catchExceptionType = array[1];
        String catchFlag = array[2];
        int tryStartLineNumber = Integer.parseInt(array[3]);
        int tryEndLineNumber = Integer.parseInt(array[4]);
        int tryMinCallId = Integer.parseInt(array[5]);
        int tryMaxCallId = Integer.parseInt(array[6]);
        int catchStartOffset = Integer.parseInt(array[7]);
        int catchEndOffset = Integer.parseInt(array[8]);
        int catchStartLineNumber = Integer.parseInt(array[9]);
        int catchEndLineNumber = Integer.parseInt(array[10]);
        int catchMinCallId = Integer.parseInt(array[11]);
        int catchMaxCallId = Integer.parseInt(array[12]);

        WriteDbData4MethodCatch writeDbData4MethodCatch = new WriteDbData4MethodCatch();
        writeDbData4MethodCatch.setRecordId(genNextRecordId());
        writeDbData4MethodCatch.setMethodHash(methodHash);
        writeDbData4MethodCatch.setSimpleClassName(dbOperWrapper.querySimpleClassName(className));
        writeDbData4MethodCatch.setMethodName(methodName);
        writeDbData4MethodCatch.setSimpleCatchExceptionType(dbOperWrapper.querySimpleClassName(catchExceptionType));
        writeDbData4MethodCatch.setCatchExceptionType(catchExceptionType);
        writeDbData4MethodCatch.setCatchFlag(catchFlag);
        writeDbData4MethodCatch.setTryStartLineNumber(tryStartLineNumber);
        writeDbData4MethodCatch.setTryEndLineNumber(tryEndLineNumber);
        writeDbData4MethodCatch.setTryMinCallId(tryMinCallId);
        writeDbData4MethodCatch.setTryMaxCallId(tryMaxCallId);
        writeDbData4MethodCatch.setCatchStartOffset(catchStartOffset);
        writeDbData4MethodCatch.setCatchEndOffset(catchEndOffset);
        writeDbData4MethodCatch.setCatchStartLineNumber(catchStartLineNumber);
        writeDbData4MethodCatch.setCatchEndLineNumber(catchEndLineNumber);
        writeDbData4MethodCatch.setCatchMinCallId(catchMinCallId);
        writeDbData4MethodCatch.setCatchMaxCallId(catchMaxCallId);
        writeDbData4MethodCatch.setFullMethod(fullMethod);
        return writeDbData4MethodCatch;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4MethodCatch data) {
        return new Object[]{
                data.getRecordId(),
                data.getMethodHash(),
                data.getSimpleClassName(),
                data.getMethodName(),
                data.getSimpleCatchExceptionType(),
                data.getCatchExceptionType(),
                data.getCatchFlag(),
                data.getTryStartLineNumber(),
                data.getTryEndLineNumber(),
                data.getTryMinCallId(),
                data.getTryMaxCallId(),
                data.getCatchStartOffset(),
                data.getCatchEndOffset(),
                data.getCatchStartLineNumber(),
                data.getCatchEndLineNumber(),
                data.getCatchMinCallId(),
                data.getCatchMaxCallId(),
                data.getFullMethod()
        };
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "完整方法（类名+方法名+参数）",
                "catch捕获的异常类型",
                "catch标志，switch: 编译器为switch生成的catch代码块，try-with-resource: 编译器为try-with-resource生成的catch代码块",
                "try代码块开始代码行号",
                "try代码块结束代码行号",
                "try代码块最小方法调用ID",
                "try代码块最大方法调用ID",
                "catch代码块开始指令偏移量",
                "catch代码块结束指令偏移量",
                "catch代码块开始代码行号",
                "catch代码块结束代码行号",
                "catch代码块最小方法调用ID",
                "catch代码块最大方法调用ID"
        };
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "方法的catch，包括catch的异常类型，try与catch代码块开始的代码行号与结束的代码行号，try与catch代码块最小及最大的方法调用ID"
        };
    }
}
