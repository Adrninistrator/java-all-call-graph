package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodThrow;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.javacg2.common.enums.JavaCG2OutPutFileTypeEnum;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;
import com.adrninistrator.javacg2.util.JavaCG2Util;

/**
 * @author adrninistrator
 * @date 2024/1/2
 * @description: 写入数据库，方法的throw信息
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCG2OutPutFileTypeEnum.OPFTE_METHOD_THROW,
        minColumnNum = 10,
        maxColumnNum = 10,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_METHOD_THROW
)
public class WriteDbHandler4MethodThrow extends AbstractWriteDbHandler<WriteDbData4MethodThrow> {

    public WriteDbHandler4MethodThrow(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4MethodThrow genData(String[] array) {
        String fullMethod = readLineData();
        String returnType = readLineData();
        String className = JavaCG2ClassMethodUtil.getClassNameFromMethod(fullMethod);
        String methodHash = JACGClassMethodUtil.genMethodHashWithLen(fullMethod, returnType);
        int throwOffset = Integer.parseInt(readLineData());
        int lineNumber = Integer.parseInt(readLineData());
        int seq = Integer.parseInt(readLineData());
        String throwExceptionType = readLineData();
        String throwFlag = readLineData();
        Integer catchStartOffset = JavaCG2Util.genIntegerFromString(readLineData());
        String catchExceptionVariableName = readLineData();
        Integer callId = JavaCG2Util.genIntegerFromString(readLineData());

        WriteDbData4MethodThrow writeDbData4MethodThrow = new WriteDbData4MethodThrow();
        writeDbData4MethodThrow.setRecordId(genNextRecordId());
        writeDbData4MethodThrow.setMethodHash(methodHash);
        writeDbData4MethodThrow.setSimpleClassName(dbOperWrapper.querySimpleClassName(className));
        writeDbData4MethodThrow.setThrowOffset(throwOffset);
        writeDbData4MethodThrow.setLineNumber(lineNumber);
        writeDbData4MethodThrow.setSeq(seq);
        writeDbData4MethodThrow.setThrowExceptionType(throwExceptionType);
        writeDbData4MethodThrow.setThrowFlag(throwFlag);
        writeDbData4MethodThrow.setCatchStartOffset(catchStartOffset);
        writeDbData4MethodThrow.setCatchExceptionVariableName(catchExceptionVariableName);
        writeDbData4MethodThrow.setCallId(callId);
        writeDbData4MethodThrow.setFullMethod(fullMethod);
        writeDbData4MethodThrow.setReturnType(returnType);
        return writeDbData4MethodThrow;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4MethodThrow data) {
        return new Object[]{
                data.getRecordId(),
                data.getMethodHash(),
                data.getSimpleClassName(),
                data.getThrowOffset(),
                data.getLineNumber(),
                data.getSeq(),
                data.getThrowExceptionType(),
                data.getThrowFlag(),
                data.getCatchStartOffset(),
                data.getCatchExceptionVariableName(),
                data.getCallId(),
                data.getFullMethod(),
                data.getReturnType()
        };
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "完整方法（类名+方法名+参数）",
                "方法返回类型，包含数组标志",
                "throw指令的偏移量",
                "throw的代码行号",
                "序号，从0开始，大于0代表有多种可能",
                "throw的异常类型",
                "throw的标志，ce:catch的异常对象，mcr:方法调用返回值，unk:未知情况",
                "抛出异常属于catch的异常对象时，对应的catch代码块开始指令偏移量",
                "抛出异常对应的catch的异常对象变量名称",
                "抛出异常属于方法调用返回值时，对应的方法调用ID"
        };
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "方法中throw的异常信息，包括抛出catch的异常对象，抛出方法调用返回值等情况"
        };
    }
}
