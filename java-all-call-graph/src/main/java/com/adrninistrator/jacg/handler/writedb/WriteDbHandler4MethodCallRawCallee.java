package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCallRawCallee;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.javacg2.common.enums.JavaCG2OutPutFileTypeEnum;

/**
 * @author adrninistrator
 * @date 2025/9/21
 * @description: 写入数据库，方法调用被调用对象的原始类型
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCG2OutPutFileTypeEnum.OPFTE_METHOD_CALL_RAW_CALLEE,
        minColumnNum = 2,
        maxColumnNum = 2,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_METHOD_CALL_RAW_CALLEE
)
public class WriteDbHandler4MethodCallRawCallee extends AbstractWriteDbHandler<WriteDbData4MethodCallRawCallee> {

    public WriteDbHandler4MethodCallRawCallee(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4MethodCallRawCallee genData(String[] array) {
        int callId = Integer.parseInt(readLineData());
        String rawCalleeClassName = readLineData();

        WriteDbData4MethodCallRawCallee writeDbData4MethodCallRawCallee = new WriteDbData4MethodCallRawCallee();
        writeDbData4MethodCallRawCallee.setCallId(callId);
        writeDbData4MethodCallRawCallee.setRawCalleeClassName(rawCalleeClassName);
        return writeDbData4MethodCallRawCallee;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4MethodCallRawCallee data) {
        return new Object[]{
                data.getCallId(),
                data.getRawCalleeClassName()
        };
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "方法调用序号，从1开始",
                "原始的被调用完整类名",
        };
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "方法调用被调用对象的原始类型表，当代码中使用的原始被调用对象与实际的不同时会记录到当前表"
        };
    }
}

