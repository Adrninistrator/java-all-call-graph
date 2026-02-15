package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCallInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.common.enums.JavaCG2MethodCallInfoTypeEnum;
import com.adrninistrator.javacg2.common.enums.JavaCG2OutPutFileTypeEnum;
import com.adrninistrator.javacg2.util.JavaCG2Util;

import java.util.Set;

/**
 * @author adrninistrator
 * @date 2022/11/16
 * @description: 写入数据库，方法调用信息
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCG2OutPutFileTypeEnum.OPFTE_METHOD_CALL_INFO,
        minColumnNum = 12,
        maxColumnNum = 12,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_METHOD_CALL_INFO
)
public class WriteDbHandler4MethodCallInfo extends AbstractWriteDbHandler<WriteDbData4MethodCallInfo> {
    // 被调用对象及参数存在信息的call_id
    private Set<Integer> withInfoCallIdSet;

    public WriteDbHandler4MethodCallInfo(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4MethodCallInfo genData(String[] array) {
        int callId = Integer.parseInt(readLineData());
        String objArgsSeq = readLineData();
        String seq = readLineData();
        String type = readLineData();
        int arrayFlag = Integer.parseInt(readLineData());
        String arrayCollectionSeq = readLineData();
        String arrayDimensions = readLineData();
        String arrayIndex = readLineData();
        String valueType = readLineData();
        String value = readLineData();
        if (JavaCG2MethodCallInfoTypeEnum.MCIT_BASE64_VALUE.getType().equals(type)) {
            // bv类型数据需要进行base64解码
            value = JavaCG2Util.base64Decode(value);
        }
        if (usePg) {
            // 使用PostgreSQL数据库时，不支持插入的字符串数据包含0x00
            value = value.replace("\u0000", "");
        }

        String callerFullMethod = readLineData();
        String returnType = readLineData();
        // 记录被调用对象及参数存在信息的call_id
        withInfoCallIdSet.add(callId);
        WriteDbData4MethodCallInfo writeDbData4MethodCallInfo = new WriteDbData4MethodCallInfo();
        writeDbData4MethodCallInfo.setRecordId(genNextRecordId());
        writeDbData4MethodCallInfo.setCallId(callId);
        writeDbData4MethodCallInfo.setObjArgsSeq(Integer.parseInt(objArgsSeq));
        writeDbData4MethodCallInfo.setSeq(Integer.parseInt(seq));
        writeDbData4MethodCallInfo.setType(type);
        writeDbData4MethodCallInfo.setArrayFlag(arrayFlag);
        writeDbData4MethodCallInfo.setArrayCollectionSeq("".equals(arrayCollectionSeq) ? JavaCG2Constants.NO_ARRAY_ELEMENT_COLLECTION_SEQ : Integer.parseInt(arrayCollectionSeq));
        writeDbData4MethodCallInfo.setArrayDimensions("".equals(arrayDimensions) ? 0 : Integer.parseInt(arrayDimensions));
        writeDbData4MethodCallInfo.setArrayIndex("".equals(arrayIndex) ? null : arrayIndex);
        writeDbData4MethodCallInfo.setValueType(valueType);
        writeDbData4MethodCallInfo.setTheValue(value);
        writeDbData4MethodCallInfo.setCallerMethodHash(JACGClassMethodUtil.genMethodHashWithLen(callerFullMethod, returnType));
        return writeDbData4MethodCallInfo;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4MethodCallInfo data) {
        return new Object[]{
                data.getRecordId(),
                data.getCallId(),
                data.getObjArgsSeq(),
                data.getSeq(),
                data.getType(),
                data.getArrayFlag(),
                data.getArrayCollectionSeq(),
                data.getArrayDimensions(),
                data.getArrayIndex(),
                data.getValueType(),
                data.getTheValue(),
                data.getCallerMethodHash()
        };
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "方法调用序号，从1开始",
                "被调用对象或参数序号，0代表被调用对象，1开始为参数",
                "序号，从0开始，大于0代表有多种可能",
                "类型，含义参考 JavaCG2MethodCallInfoTypeEnum 类",
                "是否为数组格式，1:是，0:否",
                "数组值组合序号，从0开始，非数组时为-1",
                "数组维度，从1开始，非数组时为0",
                "数组下标，逗号分隔，如\"0\"、\"0,1\"、\"0,1,2\"",
                "值的类型，含义参考 JavaCG2ConstantTypeEnum 类",
                "对应的值",
                "调用方，完整方法（类名+方法名+参数）",
                "方法返回类型，包含数组标志"
        };
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "方法调用信息，包括方法调用中被调用对象与参数可能的类型以及值"
        };
    }

    public void setWithInfoCallIdSet(Set<Integer> withInfoCallIdSet) {
        this.withInfoCallIdSet = withInfoCallIdSet;
    }
}
