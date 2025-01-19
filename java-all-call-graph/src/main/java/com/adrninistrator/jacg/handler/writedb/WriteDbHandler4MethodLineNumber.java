package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodLineNumber;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg2.common.enums.JavaCG2OutPutFileTypeEnum;

/**
 * @author adrninistrator
 * @date 2022/11/15
 * @description: 写入数据库，方法行号
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCG2OutPutFileTypeEnum.OPFTE_METHOD_LINE_NUMBER,
        minColumnNum = 3,
        maxColumnNum = 3,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_METHOD_LINE_NUMBER
)
public class WriteDbHandler4MethodLineNumber extends AbstractWriteDbHandler<WriteDbData4MethodLineNumber> {

    public WriteDbHandler4MethodLineNumber(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4MethodLineNumber genData(String[] array) {
        String fullMethod = readLineData();
        // 根据完整方法前缀判断是否需要处理
        if (!isAllowedClassPrefix(fullMethod)) {
            return null;
        }

        String minLineNumber = readLineData();
        String maxLineNumber = readLineData();
        String methodHash = JACGUtil.genHashWithLen(fullMethod);
        String className = JACGClassMethodUtil.getClassNameFromMethod(fullMethod);
        String simpleClassName = dbOperWrapper.querySimpleClassName(className);
        String methodName = JACGClassMethodUtil.getMethodNameFromFull(fullMethod);
        WriteDbData4MethodLineNumber methodLineNumber = new WriteDbData4MethodLineNumber();
        methodLineNumber.setRecordId(genNextRecordId());
        methodLineNumber.setMethodHash(methodHash);
        methodLineNumber.setSimpleClassName(simpleClassName);
        methodLineNumber.setMethodName(methodName);
        methodLineNumber.setMinLineNumber(Integer.parseInt(minLineNumber));
        methodLineNumber.setMaxLineNumber(Integer.parseInt(maxLineNumber));
        methodLineNumber.setFullMethod(fullMethod);
        return methodLineNumber;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4MethodLineNumber data) {
        return new Object[]{
                data.getRecordId(),
                data.getMethodHash(),
                data.getSimpleClassName(),
                data.getMethodName(),
                data.getMinLineNumber(),
                data.getMaxLineNumber(),
                data.getFullMethod()
        };
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "完整方法（类名+方法名+参数）",
                "起始代码行号",
                "结束代码行号"
        };
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "方法代码行号信息，包括起始行号与结束行号"
        };
    }
}
