package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCallStaticField;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.javacg2.common.enums.JavaCG2OutPutFileTypeEnum;

/**
 * @author adrninistrator
 * @date 2024/2/3
 * @description: 写入数据库，方法调用使用静态字段信息
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCG2OutPutFileTypeEnum.OPFTE_METHOD_CALL_STATIC_FIELD,
        minColumnNum = 8,
        maxColumnNum = 8,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_METHOD_CALL_STATIC_FIELD
)
public class WriteDbHandler4MethodCallStaticField extends AbstractWriteDbHandler4MethodCallClassField<WriteDbData4MethodCallStaticField> {

    public WriteDbHandler4MethodCallStaticField(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4MethodCallStaticField genData(String[] array) {
        WriteDbData4MethodCallStaticField writeDbData4MethodCallStaticField = new WriteDbData4MethodCallStaticField();
        fillInBaseWriteDbData4MethodCallClassField(writeDbData4MethodCallStaticField);
        return writeDbData4MethodCallStaticField;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4MethodCallStaticField data) {
        return genObjectArrayBase(data);
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "方法调用序号，从1开始",
                "被调用对象或参数序号，",
                "序号，从0开始，大于0代表有多种可能",
                "静态字段所在类完整类名",
                "静态字段名称",
                "静态字段类型",
                "调用方，完整方法（类名+方法名+参数）",
                "调用方，方法返回类型，包含数组标志",
        };
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "方法调用使用静态字段信息，包括方法调用中被调用对象与参数可能使用的静态字段信息",
                "包括静态字段所在的类名、字段名称、字段类型等"
        };
    }
}
