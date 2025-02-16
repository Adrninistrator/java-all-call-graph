package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4EnumInitArgField;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.javacg2.common.enums.JavaCG2OutPutFileTypeEnum;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;

/**
 * @author adrninistrator
 * @date 2025/1/17
 * @description: 写入数据库，枚举类构造函数参数与字段赋值关系
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCG2OutPutFileTypeEnum.OPFTE_ENUM_INIT_ARG_FIELD,
        minColumnNum = 4,
        maxColumnNum = 4,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_ENUM_INIT_ARG_FIELD
)
public class WriteDbHandler4EnumInitArgField extends AbstractWriteDbHandler<WriteDbData4EnumInitArgField> {

    public WriteDbHandler4EnumInitArgField(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4EnumInitArgField genData(String[] array) {
        String fullMethod = readLineData();
        String className = JavaCG2ClassMethodUtil.getClassNameFromMethod(fullMethod);
        int argSeq = Integer.parseInt(readLineData());
        String fieldType = readLineData();
        String fieldName = readLineData();

        WriteDbData4EnumInitArgField enumInitArgField = new WriteDbData4EnumInitArgField();
        enumInitArgField.setRecordId(genNextRecordId());
        enumInitArgField.setSimpleClassName(dbOperWrapper.querySimpleClassName(className));
        enumInitArgField.setArgSeq(argSeq);
        enumInitArgField.setFieldType(fieldType);
        enumInitArgField.setFieldName(fieldName);
        enumInitArgField.setClassName(className);
        enumInitArgField.setFullMethod(fullMethod);
        return enumInitArgField;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4EnumInitArgField data) {
        return new Object[]{
                data.getRecordId(),
                data.getSimpleClassName(),
                data.getArgSeq(),
                data.getFieldType(),
                data.getFieldName(),
                data.getClassName(),
                data.getFullMethod()
        };
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "枚举类构造函数完整方法（类名+方法名+参数）",
                "枚举类构造函数用于赋值的参数序号（从1开始）",
                "枚举类构造函数被赋值的字段类型",
                "枚举类构造函数被赋值的字段名"
        };
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "枚举类构造函数参数与字段赋值关系，即枚举类的构造函数中会将每个参数赋值到哪个字段"
        };
    }
}
