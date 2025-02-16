package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCallInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.util.JACGUtil;
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
        minColumnNum = 8,
        maxColumnNum = 8,
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
        String callerFullMethod = array[7];
        int callId = Integer.parseInt(array[0]);
        String objArgsSeq = array[1];
        String seq = array[2];
        String type = array[3];
        int arrayFlag = Integer.parseInt(array[4]);
        String valueType = array[5];
        String value = array[6];
        if (JavaCG2MethodCallInfoTypeEnum.MCIT_BASE64_VALUE.getType().equals(type)) {
            // bv类型数据需要进行base64解码
            value = JavaCG2Util.base64Decode(value);
        }

        // 记录被调用对象及参数存在信息的call_id
        withInfoCallIdSet.add(callId);
        WriteDbData4MethodCallInfo writeDbData4MethodCallInfo = new WriteDbData4MethodCallInfo();
        writeDbData4MethodCallInfo.setRecordId(genNextRecordId());
        writeDbData4MethodCallInfo.setCallId(callId);
        writeDbData4MethodCallInfo.setObjArgsSeq(Integer.parseInt(objArgsSeq));
        writeDbData4MethodCallInfo.setSeq(Integer.parseInt(seq));
        writeDbData4MethodCallInfo.setCallerMethodHash(JACGUtil.genHashWithLen(callerFullMethod));
        writeDbData4MethodCallInfo.setType(type);
        writeDbData4MethodCallInfo.setArrayFlag(arrayFlag);
        writeDbData4MethodCallInfo.setValueType(valueType);
        writeDbData4MethodCallInfo.setTheValue(value);
        return writeDbData4MethodCallInfo;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4MethodCallInfo data) {
        return new Object[]{
                data.getRecordId(),
                data.getCallId(),
                data.getObjArgsSeq(),
                data.getSeq(),
                data.getCallerMethodHash(),
                data.getType(),
                data.getArrayFlag(),
                data.getValueType(),
                data.getTheValue()
        };
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "方法调用序号，从1开始",
                "被调用对象或参数序号，",
                "序号，从0开始，大于0代表有多种可能",
                "类型，含义参考 JavaCG2MethodCallInfoTypeEnum 类",
                "是否为数组格式，1:是，0:否",
                "值的类型，含义参考 JavaCG2ConstantTypeEnum 类",
                "对应的值",
                "调用方，完整方法（类名+方法名+参数）"
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
