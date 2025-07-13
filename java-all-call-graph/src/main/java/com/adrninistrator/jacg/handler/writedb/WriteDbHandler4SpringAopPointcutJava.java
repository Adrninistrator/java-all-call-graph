package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4SpringAopPointcut;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.common.enums.JavaCG2OutPutFileTypeEnum;
import com.adrninistrator.javacg2.common.enums.JavaCG2YesNoEnum;
import com.adrninistrator.javacg2.util.JavaCG2Util;

/**
 * @author adrninistrator
 * @date 2025/6/29
 * @description: 写入数据库，Spring AOP pointcut信息，在Java代码中定义
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCG2OutPutFileTypeEnum.OPFTE_SPRING_AOP_POINTCUT_JAVA,
        minColumnNum = 3,
        maxColumnNum = 3,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_SPRING_AOP_POINTCUT
)
public class WriteDbHandler4SpringAopPointcutJava extends AbstractWriteDbHandler<WriteDbData4SpringAopPointcut> {

    public WriteDbHandler4SpringAopPointcutJava(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4SpringAopPointcut genData(String[] array) {
        boolean base64 = JavaCG2YesNoEnum.isYes(readLineData());
        String expression = readLineData();
        if (base64) {
            expression = JavaCG2Util.base64Decode(expression);
        }
        String fullMethod = readLineData();
        WriteDbData4SpringAopPointcut writeDbData4SpringAopPointcut = new WriteDbData4SpringAopPointcut();
        writeDbData4SpringAopPointcut.setRecordId(genNextRecordId());
        writeDbData4SpringAopPointcut.setType(JavaCG2Constants.FILE_KEY_SPRING_DEFINE_IN_JAVA);
        writeDbData4SpringAopPointcut.setXmlPointcutId("");
        writeDbData4SpringAopPointcut.setExpression(expression);
        writeDbData4SpringAopPointcut.setFullMethod(fullMethod);
        writeDbData4SpringAopPointcut.setDefineXmlPath("");
        return writeDbData4SpringAopPointcut;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4SpringAopPointcut data) {
        return JACGSqlUtil.genSpringAopPointcutArray(data);
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "pointcut表达式是否为base64格式",
                "pointcut表达式",
                "pointcut在Java代码中定义时所在的完整方法"
        };
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "包括pointcut在Java代码中定义的表达式、所在的完整方法等"
        };
    }
}
