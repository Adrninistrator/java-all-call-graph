package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData;
import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData4MethodCallClassField;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;

/**
 * @author adrninistrator
 * @date 2025/5/28
 * @description: 写入数据库，方法调用使用静态字段信息
 */
public abstract class AbstractWriteDbHandler4MethodCallClassField<T extends BaseWriteDbData> extends AbstractWriteDbHandler<T> {

    public AbstractWriteDbHandler4MethodCallClassField(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    /**
     * 读取文件内容并填充对象
     *
     * @param methodCallClassField
     */
    protected void fillInBaseWriteDbData4MethodCallClassField(BaseWriteDbData4MethodCallClassField methodCallClassField) {
        int callId = Integer.parseInt(readLineData());
        String objArgsSeq = readLineData();
        String seq = readLineData();
        String className = readLineData();
        String fieldName = readLineData();
        String fieldType = readLineData();
        String callerFullMethod = readLineData();
        String callerReturnType = readLineData();

        methodCallClassField.setRecordId(genNextRecordId());
        methodCallClassField.setCallId(callId);
        methodCallClassField.setObjArgsSeq(Integer.parseInt(objArgsSeq));
        methodCallClassField.setSeq(Integer.parseInt(seq));
        methodCallClassField.setCallerMethodHash(JACGClassMethodUtil.genMethodHashWithLen(callerFullMethod, callerReturnType));
        methodCallClassField.setSimpleClassName(dbOperWrapper.querySimpleClassName(className));
        methodCallClassField.setFieldName(fieldName);
        methodCallClassField.setSimpleFieldType(dbOperWrapper.querySimpleClassName(fieldType));
        methodCallClassField.setClassName(className);
        methodCallClassField.setFieldType(fieldType);
    }

    protected Object[] genObjectArrayBase(BaseWriteDbData4MethodCallClassField data) {
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
}
