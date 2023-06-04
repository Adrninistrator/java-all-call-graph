package com.adrninistrator.jacg.handler.write_db;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.write_db.WriteDbData4MethodLineNumber;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg.common.enums.JavaCGOutPutFileTypeEnum;
import com.adrninistrator.javacg.dto.output.JavaCGOutputInfo;

/**
 * @author adrninistrator
 * @date 2022/11/15
 * @description: 写入数据库，方法行号
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCGOutPutFileTypeEnum.OPFTE_METHOD_LINE_NUMBER,
        minColumnNum = 3,
        maxColumnNum = 3,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_METHOD_LINE_NUMBER
)
public class WriteDbHandler4MethodLineNumber extends AbstractWriteDbHandler<WriteDbData4MethodLineNumber> {
    public WriteDbHandler4MethodLineNumber(JavaCGOutputInfo javaCGOutputInfo) {
        super(javaCGOutputInfo);
    }

    @Override
    protected WriteDbData4MethodLineNumber genData(String[] array) {
        String fullMethod = array[0];
        // 根据完整方法前缀判断是否需要处理
        if (!isAllowedClassPrefix(fullMethod)) {
            return null;
        }

        String minLineNumber = array[1];
        String maxLineNumber = array[2];
        String methodHash = JACGUtil.genHashWithLen(fullMethod);
        String className = JACGClassMethodUtil.getClassNameFromMethod(fullMethod);
        String simpleClassName = dbOperWrapper.getSimpleClassName(className);
        return new WriteDbData4MethodLineNumber(methodHash, simpleClassName, Integer.parseInt(minLineNumber), Integer.parseInt(maxLineNumber), fullMethod);
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4MethodLineNumber data) {
        return new Object[]{
                data.getMethodHash(),
                data.getSimpleClassName(),
                data.getMinLineNumber(),
                data.getMaxLineNumber(),
                data.getFullMethod()
        };
    }
}
