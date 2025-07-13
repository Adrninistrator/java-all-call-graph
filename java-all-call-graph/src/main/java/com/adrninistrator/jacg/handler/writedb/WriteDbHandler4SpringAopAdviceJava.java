package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4SpringAopAdvice;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGSpringUtil;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.common.enums.JavaCG2OutPutFileTypeEnum;
import com.adrninistrator.javacg2.common.enums.JavaCG2YesNoEnum;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.aspectj.weaver.AdviceKind;

/**
 * @author adrninistrator
 * @date 2025/6/29
 * @description: 写入数据库，Spring AOP advice信息，在Java代码中定义
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCG2OutPutFileTypeEnum.OPFTE_SPRING_AOP_ADVICE_JAVA,
        minColumnNum = 6,
        maxColumnNum = 6,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_SPRING_AOP_ADVICE
)
public class WriteDbHandler4SpringAopAdviceJava extends AbstractWriteDbHandler<WriteDbData4SpringAopAdvice> {

    public WriteDbHandler4SpringAopAdviceJava(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4SpringAopAdvice genData(String[] array) {
        String adviceFullMethod = readLineData();
        String adviceMethodReturnType = readLineData();
        String adviceType = readLineData();
        boolean base64 = JavaCG2YesNoEnum.isYes(readLineData());
        String expression = readLineData();
        if (base64) {
            expression = JavaCG2Util.base64Decode(expression);
        }
        int aspectOrder = Integer.parseInt(readLineData());
        WriteDbData4SpringAopAdvice writeDbData4SpringAopAdvice = new WriteDbData4SpringAopAdvice();
        writeDbData4SpringAopAdvice.setRecordId(genNextRecordId());
        writeDbData4SpringAopAdvice.setType(JavaCG2Constants.FILE_KEY_SPRING_DEFINE_IN_JAVA);
        writeDbData4SpringAopAdvice.setXmlAspectId("");
        writeDbData4SpringAopAdvice.setXmlAspectMethodName("");
        AdviceKind adviceKind = JACGSpringUtil.getAdviceKindFromAnnotationSCN(adviceType);
        writeDbData4SpringAopAdvice.setAdviceType(adviceKind.getName());
        writeDbData4SpringAopAdvice.setXmlPointcutRef("");
        writeDbData4SpringAopAdvice.setExpression(expression);
        writeDbData4SpringAopAdvice.setAspectOrder(aspectOrder);
        writeDbData4SpringAopAdvice.setAdviceFullMethod(adviceFullMethod);
        writeDbData4SpringAopAdvice.setAdviceMethodReturnType(adviceMethodReturnType);
        String adviceMethodHash = JACGClassMethodUtil.genMethodHashWithLen(adviceFullMethod, adviceMethodReturnType);
        writeDbData4SpringAopAdvice.setAdviceMethodHash(adviceMethodHash);
        writeDbData4SpringAopAdvice.setAspectClassName(JavaCG2ClassMethodUtil.getClassNameFromMethod(adviceFullMethod));
        writeDbData4SpringAopAdvice.setDefineXmlPath("");
        return writeDbData4SpringAopAdvice;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4SpringAopAdvice data) {
        return JACGSqlUtil.genSpringAopAdviceArray(data);
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "advice的完整方法",
                "advice的方法返回类型",
                "Java代码中定义的advice注解简单类名",
                "pointcut表达式是否为base64格式",
                "pointcut表达式",
                "aspect排序数值"
        };
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "包括advice在Java代码中定义时的完整方法、advice注解简单类名、表达式等"
        };
    }
}
