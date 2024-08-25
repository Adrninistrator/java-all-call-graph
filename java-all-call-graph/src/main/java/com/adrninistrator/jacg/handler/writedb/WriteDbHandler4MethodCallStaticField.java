package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCallStaticField;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg.common.enums.JavaCGOutPutFileTypeEnum;

/**
 * @author adrninistrator
 * @date 2024/2/3
 * @description: 写入数据库，方法调用使用静态字段信息
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCGOutPutFileTypeEnum.OPFTE_METHOD_CALL_STATIC_FIELD,
        minColumnNum = 7,
        maxColumnNum = 7,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_METHOD_CALL_STATIC_FIELD
)
public class WriteDbHandler4MethodCallStaticField extends AbstractWriteDbHandler<WriteDbData4MethodCallStaticField> {

    public WriteDbHandler4MethodCallStaticField(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4MethodCallStaticField genData(String[] array) {
        String callerFullMethod = array[6];
        // 根据完整方法前缀判断是否需要处理
        if (!isAllowedClassPrefix(callerFullMethod)) {
            return null;
        }

        int callId = Integer.parseInt(array[0]);
        String objArgsSeq = array[1];
        String seq = array[2];
        String className = array[3];
        String fieldName = array[4];
        String fieldType = array[5];

        WriteDbData4MethodCallStaticField writeDbData4MethodCallStaticField = new WriteDbData4MethodCallStaticField();
        writeDbData4MethodCallStaticField.setRecordId(genNextRecordId());
        writeDbData4MethodCallStaticField.setCallId(callId);
        writeDbData4MethodCallStaticField.setObjArgsSeq(Integer.parseInt(objArgsSeq));
        writeDbData4MethodCallStaticField.setSeq(Integer.parseInt(seq));
        writeDbData4MethodCallStaticField.setCallerMethodHash(JACGUtil.genHashWithLen(callerFullMethod));
        writeDbData4MethodCallStaticField.setSimpleClassName(dbOperWrapper.querySimpleClassName(className));
        writeDbData4MethodCallStaticField.setFieldName(fieldName);
        writeDbData4MethodCallStaticField.setSimpleFieldType(dbOperWrapper.querySimpleClassName(fieldType));
        writeDbData4MethodCallStaticField.setClassName(className);
        writeDbData4MethodCallStaticField.setFieldType(fieldType);

        return writeDbData4MethodCallStaticField;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4MethodCallStaticField data) {
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
                data.getFieldType()
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
                "调用方，完整方法（类名+方法名+参数）"
        };
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "方法调用使用静态字段信息，包括方法调用中被调用对象与参数可能使用的静态字段信息",
                "包括静态字段所在的类名、字段名称、字段类型等"
        };
    }
}
