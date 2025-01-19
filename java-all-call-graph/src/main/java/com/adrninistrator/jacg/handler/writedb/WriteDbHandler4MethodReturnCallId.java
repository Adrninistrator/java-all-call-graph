package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodReturnCallId;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg2.common.enums.JavaCG2OutPutFileTypeEnum;

/**
 * @author adrninistrator
 * @date 2023/10/15
 * @description: 写入数据库，方法返回值对应的方法调用序号
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCG2OutPutFileTypeEnum.OPFTE_METHOD_RETURN_CALL_ID,
        minColumnNum = 3,
        maxColumnNum = 3,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_METHOD_RETURN_CALL_ID
)
public class WriteDbHandler4MethodReturnCallId extends AbstractWriteDbHandler<WriteDbData4MethodReturnCallId> {

    public WriteDbHandler4MethodReturnCallId(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4MethodReturnCallId genData(String[] array) {
        String fullMethod = readLineData();
        // 根据完整方法前缀判断是否需要处理
        if (!isAllowedClassPrefix(fullMethod)) {
            return null;
        }

        int returnCallId = Integer.parseInt(readLineData());
        String methodHash = JACGUtil.genHashWithLen(fullMethod);
        int equivalentConversion = Integer.parseInt(readLineData());
        WriteDbData4MethodReturnCallId methodReturnCallId = new WriteDbData4MethodReturnCallId();
        methodReturnCallId.setRecordId(genNextRecordId());
        methodReturnCallId.setMethodHash(methodHash);
        methodReturnCallId.setReturnCallId(returnCallId);
        methodReturnCallId.setFullMethod(fullMethod);
        methodReturnCallId.setEquivalentConversion(equivalentConversion);
        return methodReturnCallId;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4MethodReturnCallId data) {
        return new Object[]{
                data.getRecordId(),
                data.getMethodHash(),
                data.getReturnCallId(),
                data.getFullMethod(),
                data.getEquivalentConversion()
        };
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "完整方法（类名+方法名+参数）",
                "方法返回值对应的方法调用序号，从1开始",
                "是否返回等值转换前的方法调用，1:是，0:否"
        };
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "方法返回值对应的方法调用序号信息，即调用方法以某个被调用方法的返回值作为自己的返回值时，记录两个方法的对应关系"
        };
    }
}
