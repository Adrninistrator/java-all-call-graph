package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4FieldUsageOther;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg2.common.enums.JavaCG2OutPutFileTypeEnum;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;

/**
 * @author adrninistrator
 * @date 2025/7/25
 * @description: 写入数据库，使用其他类中字段的使用情况
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCG2OutPutFileTypeEnum.OPFTE_FIELD_USAGE_OTHER,
        minColumnNum = 10,
        maxColumnNum = 10,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_FIELD_USAGE_OTHER
)
public class WriteDbHandler4FieldUsageOther extends AbstractWriteDbHandler<WriteDbData4FieldUsageOther> {

    public WriteDbHandler4FieldUsageOther(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4FieldUsageOther genData(String[] array) {
        String fullMethod = readLineData();
        String className = JavaCG2ClassMethodUtil.getClassNameFromMethod(fullMethod);
        String simpleClassName = dbOperWrapper.querySimpleClassName(className);
        String methodReturnType = readLineData();
        String methodHash = JACGClassMethodUtil.genMethodHashWithLen(fullMethod, methodReturnType);
        int staticFlag = Integer.parseInt(readLineData());
        int getOrPut = Integer.parseInt(readLineData());
        String fieldInClassName = readLineData();
        String fieldInSimpleClassName = dbOperWrapper.querySimpleClassName(fieldInClassName);
        String fieldName = readLineData();
        String fieldType = readLineData();
        int lineNumber = Integer.parseInt(readLineData());
        String classJarNumStr = readLineData();
        String fieldJarNumStr = readLineData();
        Integer classJarNum = JACGUtil.parseJarNum(classJarNumStr);
        Integer fieldJarNum = JACGUtil.parseJarNum(fieldJarNumStr);
        WriteDbData4FieldUsageOther writeDbData4FieldUsageOther = new WriteDbData4FieldUsageOther();
        writeDbData4FieldUsageOther.setRecordId(genNextRecordId());
        writeDbData4FieldUsageOther.setFullMethod(fullMethod);
        writeDbData4FieldUsageOther.setMethodReturnType(methodReturnType);
        writeDbData4FieldUsageOther.setStaticFlag(staticFlag);
        writeDbData4FieldUsageOther.setGetOrPut(getOrPut);
        writeDbData4FieldUsageOther.setFieldInSimpleClassName(fieldInSimpleClassName);
        writeDbData4FieldUsageOther.setFieldName(fieldName);
        writeDbData4FieldUsageOther.setFieldType(fieldType);
        writeDbData4FieldUsageOther.setLineNumber(lineNumber);
        writeDbData4FieldUsageOther.setSimpleClassName(simpleClassName);
        writeDbData4FieldUsageOther.setClassName(className);
        writeDbData4FieldUsageOther.setMethodHash(methodHash);
        writeDbData4FieldUsageOther.setFieldInClassName(fieldInClassName);
        writeDbData4FieldUsageOther.setClassJarNum(classJarNum);
        writeDbData4FieldUsageOther.setFieldJarNum(fieldJarNum);
        return writeDbData4FieldUsageOther;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4FieldUsageOther data) {
        return new Object[]{
                data.getRecordId(),
                data.getFullMethod(),
                data.getMethodReturnType(),
                data.getStaticFlag(),
                data.getGetOrPut(),
                data.getFieldInSimpleClassName(),
                data.getFieldName(),
                data.getFieldType(),
                data.getLineNumber(),
                data.getSimpleClassName(),
                data.getClassName(),
                data.getMethodHash(),
                data.getFieldInClassName(),
                data.getClassJarNum(),
                data.getFieldJarNum()
        };
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "完整方法（类名+方法名+参数）",
                "方法返回类型类名（包含数组标志）",
                "static标志，1:是，0:否",
                "使用字段时get还是put，1:get，0:put",
                "被使用的字段所在的类名",
                "被使用的字段名称",
                "被使用的字段类型（包含数组标志）",
                "使用字段的源代码行号",
                "类所在的jar文件序号",
                "被使用的字段所在的jar文件序号"
        };
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "使用其他类中字段的使用情况，包括字段所在的类名、名称、类型、static标志、使用字段时get还是put等"
        };
    }
}

