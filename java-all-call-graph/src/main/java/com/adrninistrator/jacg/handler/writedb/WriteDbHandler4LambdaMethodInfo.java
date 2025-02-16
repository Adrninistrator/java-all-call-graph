package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4LambdaMethodInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.util.JACGStreamUtil;
import com.adrninistrator.javacg2.common.enums.JavaCG2OutPutFileTypeEnum;
import com.adrninistrator.javacg2.common.enums.JavaCG2YesNoEnum;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;

/**
 * @author adrninistrator
 * @date 2023/1/10
 * @description: 写入数据库，Lambda表达式方法信息
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCG2OutPutFileTypeEnum.OPFTE_LAMBDA_METHOD_INFO,
        minColumnNum = 2,
        maxColumnNum = 3,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_LAMBDA_METHOD_INFO
)
public class WriteDbHandler4LambdaMethodInfo extends AbstractWriteDbHandler<WriteDbData4LambdaMethodInfo> {

    public WriteDbHandler4LambdaMethodInfo(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4LambdaMethodInfo genData(String[] array) {
        int callId = Integer.parseInt(array[0]);
        String lambdaCalleeFullMethod = array[1];
        String lambdaCalleeClassName = JavaCG2ClassMethodUtil.getClassNameFromMethod(lambdaCalleeFullMethod);
        String lambdaCalleeMethodName = JavaCG2ClassMethodUtil.getMethodNameFromFull(lambdaCalleeFullMethod);

        WriteDbData4LambdaMethodInfo writeDbData4LambdaMethodInfo = new WriteDbData4LambdaMethodInfo();
        writeDbData4LambdaMethodInfo.setCallId(callId);
        writeDbData4LambdaMethodInfo.setLambdaCalleeClassName(lambdaCalleeClassName);
        writeDbData4LambdaMethodInfo.setLambdaCalleeMethodName(lambdaCalleeMethodName);
        writeDbData4LambdaMethodInfo.setLambdaCalleeFullMethod(lambdaCalleeFullMethod);

        if (array.length < 3) {
            return writeDbData4LambdaMethodInfo;
        }

        String lambdaNextFullMethod = array[2];
        String lambdaNextClassName = JavaCG2ClassMethodUtil.getClassNameFromMethod(lambdaNextFullMethod);
        String lambdaNextMethodName = JavaCG2ClassMethodUtil.getMethodNameFromFull(lambdaNextFullMethod);

        writeDbData4LambdaMethodInfo.setLambdaNextClassName(lambdaNextClassName);
        writeDbData4LambdaMethodInfo.setLambdaNextMethodName(lambdaNextMethodName);
        writeDbData4LambdaMethodInfo.setLambdaNextFullMethod(lambdaNextFullMethod);
        writeDbData4LambdaMethodInfo.setLambdaNextIsStream(JACGStreamUtil.isStreamClass(lambdaNextClassName));
        writeDbData4LambdaMethodInfo.setLambdaNextIsIntermediate(JACGStreamUtil.isStreamIntermediateMethod(lambdaNextMethodName));
        writeDbData4LambdaMethodInfo.setLambdaNextIsTerminal(JACGStreamUtil.isStreamTerminalMethod(lambdaNextMethodName));

        return writeDbData4LambdaMethodInfo;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4LambdaMethodInfo data) {
        if (data.getLambdaNextFullMethod() == null) {
            return new Object[]{
                    data.getCallId(),
                    data.getLambdaCalleeClassName(),
                    data.getLambdaCalleeMethodName(),
                    data.getLambdaCalleeFullMethod(),
                    null,
                    null,
                    null,
                    null,
                    null,
                    null
            };
        }

        return new Object[]{
                data.getCallId(),
                data.getLambdaCalleeClassName(),
                data.getLambdaCalleeMethodName(),
                data.getLambdaCalleeFullMethod(),
                data.getLambdaNextClassName(),
                data.getLambdaNextMethodName(),
                data.getLambdaNextFullMethod(),
                JavaCG2YesNoEnum.parseIntValue(data.getLambdaNextIsStream()),
                JavaCG2YesNoEnum.parseIntValue(data.getLambdaNextIsIntermediate()),
                JavaCG2YesNoEnum.parseIntValue(data.getLambdaNextIsTerminal())
        };
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "方法调用序号，从1开始",
                "Lambda表达式被调用方完整方法（类名+方法名+参数）",
                "Lambda表达式下一个被调用完整方法（类名+方法名+参数）"
        };
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "Lambda表达式方法调用相关信息"
        };
    }
}
