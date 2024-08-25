package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCallMethodCallReturn;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg.common.enums.JavaCGOutPutFileTypeEnum;

/**
 * @author adrninistrator
 * @date 2024/4/25
 * @description: 写入数据库，方法调用使用方法调用返回值
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCGOutPutFileTypeEnum.OPFTE_METHOD_CALL_METHOD_CALL_RETURN,
        minColumnNum = 6,
        maxColumnNum = 6,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_METHOD_CALL_METHOD_CALL_RETURN
)
public class WriteDbHandler4MethodCallMethodCallReturn extends AbstractWriteDbHandler<WriteDbData4MethodCallMethodCallReturn> {

    public WriteDbHandler4MethodCallMethodCallReturn(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4MethodCallMethodCallReturn genData(String[] array) {
        String calleeFullMethod = array[5];
        // 根据完整方法前缀判断是否需要处理
        if (!isAllowedClassPrefix(calleeFullMethod)) {
            return null;
        }

        int callId = Integer.parseInt(array[0]);
        String objArgsSeq = array[1];
        String seq = array[2];
        int arrayFlag = Integer.parseInt(array[3]);
        int useReturnCallId = Integer.parseInt(array[4]);
        String calleeClassName = JACGClassMethodUtil.getClassNameFromMethod(calleeFullMethod);
        String calleeMethodName = JACGClassMethodUtil.getMethodNameFromFull(calleeFullMethod);

        WriteDbData4MethodCallMethodCallReturn writeDbData4MethodCallMethodCallReturn = new WriteDbData4MethodCallMethodCallReturn();
        writeDbData4MethodCallMethodCallReturn.setRecordId(genNextRecordId());
        writeDbData4MethodCallMethodCallReturn.setCallId(callId);
        writeDbData4MethodCallMethodCallReturn.setObjArgsSeq(Integer.parseInt(objArgsSeq));
        writeDbData4MethodCallMethodCallReturn.setSeq(Integer.parseInt(seq));
        writeDbData4MethodCallMethodCallReturn.setArrayFlag(arrayFlag);
        writeDbData4MethodCallMethodCallReturn.setUseReturnCallId(useReturnCallId);
        writeDbData4MethodCallMethodCallReturn.setCalleeMethodHash(JACGUtil.genHashWithLen(calleeFullMethod));
        writeDbData4MethodCallMethodCallReturn.setCalleeSimpleClassName(dbOperWrapper.querySimpleClassName(calleeClassName));
        writeDbData4MethodCallMethodCallReturn.setCalleeMethodName(calleeMethodName);
        writeDbData4MethodCallMethodCallReturn.setCalleeFullMethod(calleeFullMethod);
        return writeDbData4MethodCallMethodCallReturn;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4MethodCallMethodCallReturn data) {
        return new Object[]{
                data.getRecordId(),
                data.getCallId(),
                data.getObjArgsSeq(),
                data.getSeq(),
                data.getArrayFlag(),
                data.getUseReturnCallId(),
                data.getCalleeMethodHash(),
                data.getCalleeSimpleClassName(),
                data.getCalleeMethodName(),
                data.getCalleeFullMethod()
        };
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "方法调用序号，从1开始",
                "被调用对象或参数序号，",
                "序号，从0开始，大于0代表有多种可能",
                "是否为数组格式，1:是，0:否",
                "返回值被使用的方法调用序号，从1开始",
                "被调用方，完整方法（类名+方法名+参数）"
        };
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "方法调用使用方法调用返回值信息，包括方法调用中被调用对象与参数可能使用的方法调用返回值信息",
                "包括返回值被使用的被调用方法完整方法、方法调用序号等"
        };
    }
}
