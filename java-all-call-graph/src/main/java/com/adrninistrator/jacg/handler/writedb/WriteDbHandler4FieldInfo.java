package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4FieldInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.javacg2.common.enums.JavaCG2OutPutFileTypeEnum;

/**
 * @author adrninistrator
 * @date 2024/1/11
 * @description:
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCG2OutPutFileTypeEnum.OPFTE_FIELD_INFO,
        minColumnNum = 12,
        maxColumnNum = 12,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_FIELD_INFO
)
public class WriteDbHandler4FieldInfo extends AbstractWriteDbHandler<WriteDbData4FieldInfo> {

    public WriteDbHandler4FieldInfo(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4FieldInfo genData(String[] array) {
        String className = readLineData();
        String simpleClassName = dbOperWrapper.querySimpleClassName(className);
        String fieldName = readLineData();
        String fieldTypeNad = readLineData();
        int arrayDimensions = Integer.parseInt(readLineData());
        String fieldCategory = readLineData();
        String modifiers = readLineData();
        int primitiveType = Integer.parseInt(readLineData());
        int staticFlag = Integer.parseInt(readLineData());
        int finalFlag = Integer.parseInt(readLineData());
        int existsGetMethod = Integer.parseInt(readLineData());
        int existsSetMethod = Integer.parseInt(readLineData());
        int existsGenericsType = Integer.parseInt(readLineData());
        WriteDbData4FieldInfo writeDbData4FieldInfo = new WriteDbData4FieldInfo();
        writeDbData4FieldInfo.setRecordId(genNextRecordId());
        writeDbData4FieldInfo.setSimpleClassName(simpleClassName);
        writeDbData4FieldInfo.setFieldName(fieldName);
        writeDbData4FieldInfo.setFieldTypeNad(fieldTypeNad);
        writeDbData4FieldInfo.setArrayDimensions(arrayDimensions);
        writeDbData4FieldInfo.setFieldCategory(fieldCategory);
        writeDbData4FieldInfo.setModifiers(modifiers);
        writeDbData4FieldInfo.setPrimitiveType(primitiveType);
        writeDbData4FieldInfo.setStaticFlag(staticFlag);
        writeDbData4FieldInfo.setFinalFlag(finalFlag);
        writeDbData4FieldInfo.setExistsGetMethod(existsGetMethod);
        writeDbData4FieldInfo.setExistsSetMethod(existsSetMethod);
        writeDbData4FieldInfo.setExistsGenericsType(existsGenericsType);
        writeDbData4FieldInfo.setClassName(className);
        return writeDbData4FieldInfo;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4FieldInfo data) {
        return new Object[]{
                data.getRecordId(),
                data.getSimpleClassName(),
                data.getFieldName(),
                data.getFieldTypeNad(),
                data.getArrayDimensions(),
                data.getFieldCategory(),
                data.getModifiers(),
                data.getPrimitiveType(),
                data.getStaticFlag(),
                data.getFinalFlag(),
                data.getExistsGetMethod(),
                data.getExistsSetMethod(),
                data.getExistsGenericsType(),
                data.getClassName()
        };
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "完整类名",
                "字段名称",
                "字段类型（不包含数组标志）",
                "字段数组类型的维度，为0代表不是数组类型",
                "字段中的泛型类型分类，J:JDK中的类型，C:自定义类型",
                "字段修饰符",
                "基本类型，1:是，0:否",
                "static标志，1:是，0:否",
                "final标志，1:是，0:否",
                "是否存在对应的get方法，1:是，0:否",
                "是否存在对应的set方法，1:是，0:否",
                "是否存在泛型类型，1:是，0:否"
        };
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "字段的信息，包括字段名称、类型、修饰符、是否存在对应的get/set方法，是否存在泛型类型等"
        };
    }
}
