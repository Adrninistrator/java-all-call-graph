package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodFinally;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg.common.enums.JavaCGOutPutFileTypeEnum;

/**
 * @author adrninistrator
 * @date 2023/12/2
 * @description: 写入数据库，方法的finally信息
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCGOutPutFileTypeEnum.OPFTE_METHOD_FINALLY,
        minColumnNum = 7,
        maxColumnNum = 7,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_METHOD_FINALLY
)
public class WriteDbHandler4MethodFinally extends AbstractWriteDbHandler<WriteDbData4MethodFinally> {

    public WriteDbHandler4MethodFinally(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4MethodFinally genData(String[] array) {
        String fullMethod = array[0];
        // 根据完整方法前缀判断是否需要处理
        if (!isAllowedClassPrefix(fullMethod)) {
            return null;
        }

        String className = JACGClassMethodUtil.getClassNameFromMethod(fullMethod);
        String methodHash = JACGUtil.genHashWithLen(fullMethod);
        String tryCatch = array[1];
        int tryCatchStartLineNumber = Integer.parseInt(array[2]);
        int tryCatchEndLineNumber = Integer.parseInt(array[3]);
        int tryCatchMinCallId = Integer.parseInt(array[4]);
        int tryCatchMaxCallId = Integer.parseInt(array[5]);
        int finallyCatchStartLineNumber = Integer.parseInt(array[6]);

        WriteDbData4MethodFinally writeDbData4MethodFinally = new WriteDbData4MethodFinally();
        writeDbData4MethodFinally.setMethodHash(methodHash);
        writeDbData4MethodFinally.setSimpleClassName(dbOperWrapper.getSimpleClassName(className));
        writeDbData4MethodFinally.setTryCatch(tryCatch);
        writeDbData4MethodFinally.setTryCatchStartLineNumber(tryCatchStartLineNumber);
        writeDbData4MethodFinally.setTryCatchEndLineNumber(tryCatchEndLineNumber);
        writeDbData4MethodFinally.setTryCatchMinCallId(tryCatchMinCallId);
        writeDbData4MethodFinally.setTryCatchMaxCallId(tryCatchMaxCallId);
        writeDbData4MethodFinally.setFinallyStartLineNumber(finallyCatchStartLineNumber);
        writeDbData4MethodFinally.setFullMethod(fullMethod);
        return writeDbData4MethodFinally;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4MethodFinally data) {
        return new Object[]{
                genNextRecordId(),
                data.getMethodHash(),
                data.getSimpleClassName(),
                data.getTryCatch(),
                data.getTryCatchStartLineNumber(),
                data.getTryCatchEndLineNumber(),
                data.getTryCatchMinCallId(),
                data.getTryCatchMaxCallId(),
                data.getFinallyStartLineNumber(),
                data.getFullMethod()
        };
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "完整方法（类名+方法名+参数）",
                "当前的finally对应try或catch",
                "try或catch代码块开始代码行号",
                "try或catch代码块结束代码行号",
                "try或catch代码块最小方法调用ID",
                "try或catch代码块最大方法调用ID",
                "finally代码块开始代码行号"
        };
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "方法的finally信息，包括try或catch开始的代码行号与结束的代码行号，try或catch代码块最小及最大的方法调用ID，finally开始的代码行号"
        };
    }
}
