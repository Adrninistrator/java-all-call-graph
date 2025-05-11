package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCatch;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.javacg2.common.enums.JavaCG2OutPutFileTypeEnum;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;

/**
 * @author adrninistrator
 * @date 2023/12/2
 * @description: 写入数据库，方法的catch信息
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCG2OutPutFileTypeEnum.OPFTE_METHOD_CATCH,
        minColumnNum = 14,
        maxColumnNum = 14,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_METHOD_CATCH
)
public class WriteDbHandler4MethodCatch extends AbstractWriteDbHandler<WriteDbData4MethodCatch> {

    public WriteDbHandler4MethodCatch(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4MethodCatch genData(String[] array) {
        String fullMethod = readLineData();
        String returnType = readLineData();
        String className = JavaCG2ClassMethodUtil.getClassNameFromMethod(fullMethod);
        String methodName = JavaCG2ClassMethodUtil.getMethodNameFromFull(fullMethod);
        String methodHash = JACGClassMethodUtil.genMethodHashWithLen(fullMethod, returnType);
        String catchExceptionType = readLineData();
        String catchFlag = readLineData();
        int tryStartLineNumber = Integer.parseInt(readLineData());
        int tryEndLineNumber = Integer.parseInt(readLineData());
        int tryMinCallId = Integer.parseInt(readLineData());
        int tryMaxCallId = Integer.parseInt(readLineData());
        int catchStartOffset = Integer.parseInt(readLineData());
        int catchEndOffset = Integer.parseInt(readLineData());
        int catchStartLineNumber = Integer.parseInt(readLineData());
        int catchEndLineNumber = Integer.parseInt(readLineData());
        int catchMinCallId = Integer.parseInt(readLineData());
        int catchMaxCallId = Integer.parseInt(readLineData());

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
        writeDbData4MethodCatch.setReturnType(returnType);
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
                data.getFullMethod(),
                data.getReturnType()
        };
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "完整方法（类名+方法名+参数）",
                "方法返回类型，包含数组标志",
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
