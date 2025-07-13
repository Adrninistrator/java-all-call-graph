package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4SpringAopAspect;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.common.enums.JavaCG2OutPutFileTypeEnum;

/**
 * @author adrninistrator
 * @date 2025/6/29
 * @description: 写入数据库，Spring AOP aspect信息，在Java代码中定义
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCG2OutPutFileTypeEnum.OPFTE_SPRING_AOP_ASPECT_JAVA,
        minColumnNum = 2,
        maxColumnNum = 2,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_SPRING_AOP_ASPECT
)
public class WriteDbHandler4SpringAopAspectJava extends AbstractWriteDbHandler<WriteDbData4SpringAopAspect> {

    public WriteDbHandler4SpringAopAspectJava(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4SpringAopAspect genData(String[] array) {
        int order = Integer.parseInt(readLineData());
        String className = readLineData();
        WriteDbData4SpringAopAspect writeDbData4SpringAopAspect = new WriteDbData4SpringAopAspect();
        writeDbData4SpringAopAspect.setRecordId(genNextRecordId());
        writeDbData4SpringAopAspect.setType(JavaCG2Constants.FILE_KEY_SPRING_DEFINE_IN_JAVA);
        writeDbData4SpringAopAspect.setXmlAspectId("");
        writeDbData4SpringAopAspect.setXmlAspectRef("");
        writeDbData4SpringAopAspect.setAspectOrder(order);
        writeDbData4SpringAopAspect.setClassName(className);
        writeDbData4SpringAopAspect.setDefineXmlPath("");
        return writeDbData4SpringAopAspect;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4SpringAopAspect data) {
        return JACGSqlUtil.genSpringAopAspectArray(data);
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "aspect排序数值",
                "aspect在Java代码中定义时所在的类名"
        };
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "包括aspect在Java代码中定义时所在的类名、aspect排序数值等"
        };
    }
}
