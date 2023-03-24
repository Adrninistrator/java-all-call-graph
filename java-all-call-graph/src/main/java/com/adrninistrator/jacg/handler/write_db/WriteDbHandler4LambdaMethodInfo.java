package com.adrninistrator.jacg.handler.write_db;

import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.write_db.WriteDbData4LambdaMethodInfo;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGStreamUtil;
import com.adrninistrator.javacg.common.enums.JavaCGYesNoEnum;

/**
 * @author adrninistrator
 * @date 2023/1/10
 * @description: 写入数据库，Lambda表达式方法信息
 */
public class WriteDbHandler4LambdaMethodInfo extends AbstractWriteDbHandler<WriteDbData4LambdaMethodInfo> {
    @Override
    protected WriteDbData4LambdaMethodInfo genData(String line) {
        String[] array = splitBetween(line, 2, 3);

        int callId = Integer.parseInt(array[0]);
        String lambdaCalleeFullMethod = array[1];
        String lambdaCalleeClassName = JACGClassMethodUtil.getClassNameFromMethod(lambdaCalleeFullMethod);
        String lambdaCalleeMethodName = JACGClassMethodUtil.getMethodNameFromFull(lambdaCalleeFullMethod);

        WriteDbData4LambdaMethodInfo writeDbData4LambdaMethodInfo = new WriteDbData4LambdaMethodInfo();
        writeDbData4LambdaMethodInfo.setCallId(callId);
        writeDbData4LambdaMethodInfo.setLambdaCalleeClassName(lambdaCalleeClassName);
        writeDbData4LambdaMethodInfo.setLambdaCalleeMethodName(lambdaCalleeMethodName);
        writeDbData4LambdaMethodInfo.setLambdaCalleeFullMethod(lambdaCalleeFullMethod);

        if (array.length < 3) {
            return writeDbData4LambdaMethodInfo;
        }

        String lambdaNextCalleeFullMethod = array[2];
        String lambdaNextCalleeClassName = JACGClassMethodUtil.getClassNameFromMethod(lambdaNextCalleeFullMethod);
        String lambdaNextCalleeMethodName = JACGClassMethodUtil.getMethodNameFromFull(lambdaNextCalleeFullMethod);

        writeDbData4LambdaMethodInfo.setLambdaNextCalleeClassName(lambdaNextCalleeClassName);
        writeDbData4LambdaMethodInfo.setLambdaNextCalleeMethodName(lambdaNextCalleeMethodName);
        writeDbData4LambdaMethodInfo.setLambdaNextCalleeFullMethod(lambdaNextCalleeFullMethod);
        writeDbData4LambdaMethodInfo.setLambdaNextIsStream(JACGStreamUtil.isStreamClass(lambdaNextCalleeClassName));
        writeDbData4LambdaMethodInfo.setLambdaNextIsIntermediate(JACGStreamUtil.isStreamIntermediateMethod(lambdaNextCalleeMethodName));
        writeDbData4LambdaMethodInfo.setLambdaNextIsTerminal(JACGStreamUtil.isStreamTerminalMethod(lambdaNextCalleeMethodName));

        return writeDbData4LambdaMethodInfo;
    }

    @Override
    protected DbTableInfoEnum chooseDbTableInfo() {
        return DbTableInfoEnum.DTIE_LAMBDA_METHOD_INFO;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4LambdaMethodInfo data) {
        if (data.getLambdaNextCalleeFullMethod() == null) {
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
                data.getLambdaNextCalleeClassName(),
                data.getLambdaNextCalleeMethodName(),
                data.getLambdaNextCalleeFullMethod(),
                JavaCGYesNoEnum.parseIntValue(data.getLambdaNextIsStream()),
                JavaCGYesNoEnum.parseIntValue(data.getLambdaNextIsIntermediate()),
                JavaCGYesNoEnum.parseIntValue(data.getLambdaNextIsTerminal())
        };
    }
}
