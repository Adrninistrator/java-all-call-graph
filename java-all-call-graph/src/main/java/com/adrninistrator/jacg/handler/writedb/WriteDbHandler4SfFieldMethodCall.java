package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4SfFieldMethodCall;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.javacg2.common.enums.JavaCG2OutPutFileTypeEnum;

/**
 * @author adrninistrator
 * @date 2023/8/4
 * @description: 写入数据库，static、final字段初始化方法信息（含枚举）
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCG2OutPutFileTypeEnum.OPFTE_SF_FIELD_METHOD_CALL,
        minColumnNum = 8,
        maxColumnNum = 8,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_SF_FIELD_METHOD_CALL
)
public class WriteDbHandler4SfFieldMethodCall extends AbstractWriteDbHandler<WriteDbData4SfFieldMethodCall> {

    public WriteDbHandler4SfFieldMethodCall(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4SfFieldMethodCall genData(String[] array) {
        String className = readLineData();
        String fieldName = readLineData();
        int seq = Integer.parseInt(readLineData());
        int callId = Integer.parseInt(readLineData());
        String fieldTypeNad = readLineData();
        String arrayDimensions = readLineData();
        String calleeClassName = readLineData();
        String calleeMethodName = readLineData();
        String simpleClassName = dbOperWrapper.querySimpleClassName(className);
        WriteDbData4SfFieldMethodCall writeDbData4SfFieldMethodCall = new WriteDbData4SfFieldMethodCall();
        writeDbData4SfFieldMethodCall.setRecordId(genNextRecordId());
        writeDbData4SfFieldMethodCall.setSimpleClassName(simpleClassName);
        writeDbData4SfFieldMethodCall.setFieldName(fieldName);
        writeDbData4SfFieldMethodCall.setSeq(seq);
        writeDbData4SfFieldMethodCall.setCallId(callId);
        writeDbData4SfFieldMethodCall.setFieldTypeNad(fieldTypeNad);
        writeDbData4SfFieldMethodCall.setArrayDimensions(Integer.parseInt(arrayDimensions));
        writeDbData4SfFieldMethodCall.setClassName(className);
        writeDbData4SfFieldMethodCall.setCalleeClassName(calleeClassName);
        writeDbData4SfFieldMethodCall.setCalleeMethodName(calleeMethodName);
        return writeDbData4SfFieldMethodCall;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4SfFieldMethodCall data) {
        return new Object[]{
                data.getRecordId(),
                data.getSimpleClassName(),
                data.getFieldName(),
                data.getSeq(),
                data.getCallId(),
                data.getFieldTypeNad(),
                data.getArrayDimensions(),
                data.getClassName(),
                data.getCalleeClassName(),
                data.getCalleeMethodName()
        };
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "字段所在的完整类名",
                "字段名称",
                "序号，从0开始，大于0代表有多种可能",
                "字段初始化对应的方法调用序号，从1开始",
                "字段类型（不包含数组标志）",
                "字段数组类型的维度，为0代表不是数组类型",
                "初始化方法被调类名",
                "初始化方法被调用方法名"
        };
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "static、final字段在初始化时使用方法调用的返回值，保存这些字段及初始化方法的信息",
                "例如： public static final ClassA = new ClassA(\"test1\", \"test2\");",
                "也支持处理枚举中的字段"
        };
    }
}
