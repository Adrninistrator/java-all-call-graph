package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCallStaticFieldMCR;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.javacg2.common.enums.JavaCG2OutPutFileTypeEnum;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;

/**
 * @author adrninistrator
 * @date 2025/6/1
 * @description: 写入数据库，方法调用使用静态字段方法调用返回值
 */

@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCG2OutPutFileTypeEnum.OPFTE_METHOD_CALL_STATIC_FIELD_MCR,
        minColumnNum = 10,
        maxColumnNum = 10,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_METHOD_CALL_STATIC_FIELD_MCR
)
public class WriteDbHandler4MethodCallStaticFieldMCR extends AbstractWriteDbHandler<WriteDbData4MethodCallStaticFieldMCR> {

    public WriteDbHandler4MethodCallStaticFieldMCR(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4MethodCallStaticFieldMCR genData(String[] array) {
        int callId = Integer.parseInt(readLineData());
        String objArgsSeq = readLineData();
        String seq = readLineData();
        String className = readLineData();
        String fieldName = readLineData();
        String fieldType = readLineData();
        String calleeFullMethod = readLineData();
        String calleeReturnType = readLineData();
        String calleeMethodName = JavaCG2ClassMethodUtil.getMethodNameFromFull(calleeFullMethod);
        String callerFullMethod = readLineData();
        String callerReturnType = readLineData();

        WriteDbData4MethodCallStaticFieldMCR writeDbData4MethodCallStaticFieldMcr = new WriteDbData4MethodCallStaticFieldMCR();
        writeDbData4MethodCallStaticFieldMcr.setRecordId(genNextRecordId());
        writeDbData4MethodCallStaticFieldMcr.setCallId(callId);
        writeDbData4MethodCallStaticFieldMcr.setObjArgsSeq(Integer.parseInt(objArgsSeq));
        writeDbData4MethodCallStaticFieldMcr.setSeq(Integer.parseInt(seq));
        writeDbData4MethodCallStaticFieldMcr.setCallerMethodHash(JACGClassMethodUtil.genMethodHashWithLen(callerFullMethod, callerReturnType));
        writeDbData4MethodCallStaticFieldMcr.setSimpleClassName(dbOperWrapper.querySimpleClassName(className));
        writeDbData4MethodCallStaticFieldMcr.setFieldName(fieldName);
        writeDbData4MethodCallStaticFieldMcr.setSimpleFieldType(dbOperWrapper.querySimpleClassName(fieldType));
        writeDbData4MethodCallStaticFieldMcr.setClassName(className);
        writeDbData4MethodCallStaticFieldMcr.setFieldType(fieldType);
        writeDbData4MethodCallStaticFieldMcr.setCalleeMethodHash(JACGClassMethodUtil.genMethodHashWithLen(calleeFullMethod, calleeReturnType));
        writeDbData4MethodCallStaticFieldMcr.setCalleeMethodName(calleeMethodName);
        writeDbData4MethodCallStaticFieldMcr.setCalleeFullMethod(calleeFullMethod);
        writeDbData4MethodCallStaticFieldMcr.setCalleeReturnType(calleeReturnType);
        return writeDbData4MethodCallStaticFieldMcr;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4MethodCallStaticFieldMCR data) {
        return new Object[]{
                data.getRecordId(),
                data.getCallId(),
                data.getObjArgsSeq(),
                data.getSeq(),
                data.getCallerMethodHash(),
                data.getSimpleClassName(),
                data.getFieldName(),
                data.getSimpleFieldType(),
                data.getClassName(),
                data.getFieldType(),
                data.getCalleeMethodHash(),
                data.getCalleeMethodName(),
                data.getCalleeFullMethod(),
                data.getCalleeReturnType()
        };
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "方法调用序号，从1开始",
                "被调用对象或参数序号，",
                "序号，从0开始，大于0代表有多种可能",
                "静态字段所在类完整类名",
                "静态字段名称",
                "静态字段类型",
                "被调用方（静态字段所在类），完整方法（类名+方法名+参数）",
                "被调用方（静态字段所在类），方法返回类型，包含数组标志",
                "调用方，完整方法（类名+方法名+参数）",
                "调用方，方法返回类型，包含数组标志",
        };
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "方法调用中，被调用对象或参数使用静态字段方法调用返回值",
                "",
                ""
        };
    }
}
