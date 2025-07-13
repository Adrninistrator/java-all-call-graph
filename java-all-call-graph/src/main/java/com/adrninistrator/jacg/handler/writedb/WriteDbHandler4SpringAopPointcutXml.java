package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4SpringAopPointcut;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.extensions.codeparser.jarentryotherfile.SpringXmlCodeParser;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.common.enums.JavaCG2YesNoEnum;
import com.adrninistrator.javacg2.util.JavaCG2Util;

/**
 * @author adrninistrator
 * @date 2025/6/29
 * @description: 写入数据库，Spring AOP pointcut信息，在XML中定义
 */
@JACGWriteDbHandler(
        readFile = true,
        otherFileName = SpringXmlCodeParser.FILE_NAME_SPRING_AOP_POINTCUT_XML,
        minColumnNum = 4,
        maxColumnNum = 4,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_SPRING_AOP_POINTCUT
)
public class WriteDbHandler4SpringAopPointcutXml extends AbstractWriteDbHandler<WriteDbData4SpringAopPointcut> {

    public WriteDbHandler4SpringAopPointcutXml(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4SpringAopPointcut genData(String[] array) {
        String xmlPointcutId = readLineData();
        boolean base64 = JavaCG2YesNoEnum.isYes(readLineData());
        String expression = readLineData();
        if (base64) {
            expression = JavaCG2Util.base64Decode(expression);
        }
        String xmlPath = readLineData();
        WriteDbData4SpringAopPointcut writeDbData4SpringAopPointcut = new WriteDbData4SpringAopPointcut();
        writeDbData4SpringAopPointcut.setRecordId(genNextRecordId());
        writeDbData4SpringAopPointcut.setType(JavaCG2Constants.FILE_KEY_SPRING_DEFINE_IN_XML);
        writeDbData4SpringAopPointcut.setXmlPointcutId(xmlPointcutId);
        writeDbData4SpringAopPointcut.setExpression(expression);
        writeDbData4SpringAopPointcut.setFullMethod("");
        writeDbData4SpringAopPointcut.setDefineXmlPath(xmlPath);
        return writeDbData4SpringAopPointcut;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4SpringAopPointcut data) {
        return JACGSqlUtil.genSpringAopPointcutArray(data);
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "XML中定义的pointcut的ID",
                "pointcut表达式是否为base64格式",
                "pointcut表达式",
                "XML文件路径"
        };
    }

    @Override
    public String chooseNotMainFileDesc() {
        return "Spring AOP pointcut信息，在XML中定义";
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "包括XML中定义的pointcut的ID、表达式、XML文件路径等"
        };
    }
}
