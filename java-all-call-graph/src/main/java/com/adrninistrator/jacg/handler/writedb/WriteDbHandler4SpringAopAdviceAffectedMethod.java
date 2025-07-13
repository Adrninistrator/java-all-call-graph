package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.WriteDbHandlerWriteFileEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4SpringAopAdviceAffectedMethod;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;

/**
 * @author adrninistrator
 * @date 2025/7/9
 * @description: 写入数据库，Spring AOP advice影响的方法信息
 */
@JACGWriteDbHandler(
        readFile = false,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_SPRING_AOP_ADVICE_AFFECTED_METHOD,
        writeFileEnum = WriteDbHandlerWriteFileEnum.WDHWFE_SPRING_AOP_ADVICE_AFFECTED_METHOD
)
public class WriteDbHandler4SpringAopAdviceAffectedMethod extends AbstractWriteDbHandler<WriteDbData4SpringAopAdviceAffectedMethod> {

    public WriteDbHandler4SpringAopAdviceAffectedMethod(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4SpringAopAdviceAffectedMethod data) {
        return new Object[]{
                data.getRecordId(),
                data.getType(),
                data.getXmlAspectId(),
                data.getXmlAspectMethodName(),
                data.getAdviceType(),
                data.getXmlPointcutRef(),
                data.getExpression(),
                data.getAspectOrder(),
                data.getAdviceFullMethod(),
                data.getAdviceMethodReturnType(),
                data.getAdviceMethodHash(),
                data.getAspectClassName(),
                data.getDefineXmlPath(),
                data.getUnderlyingExpression(),
                data.getAffectedFullMethod(),
                data.getAffectedMethodReturnType(),
                data.getAffectedMethodHash()
        };
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "记录id，从1开始",
                "类型，j: 在Java代码中定义，x: 在XML文件中定义",
                "XML中定义的aspect的ID",
                "XML中定义的aspect的方法名称",
                "advice类型",
                "XML中的pointcut-ref名称",
                "pointcut表达式",
                "aspect排序数值",
                "advice的完整方法",
                "advice方法的返回类型",
                "advice方法hash+字节数",
                "对应aspect的类名",
                "在XML中定义时对应的文件路径",
                "底层的pointcut表达式",
                "影响的完整方法",
                "影响的方法的返回类型",
                "影响的方法hash+字节数",
        };
    }

    @Override
    public String chooseNotMainFileDesc() {
        return "Spring AOP advice影响的方法信息";
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "包括advice信息，及advice影响的方法信息"
        };
    }
}
