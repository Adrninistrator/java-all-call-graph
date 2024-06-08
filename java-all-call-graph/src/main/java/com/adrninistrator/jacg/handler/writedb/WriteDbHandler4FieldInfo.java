package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4FieldInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.javacg.common.enums.JavaCGOutPutFileTypeEnum;

/**
 * @author adrninistrator
 * @date 2024/1/11
 * @description:
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCGOutPutFileTypeEnum.OPFTE_FIELD_INFO,
        minColumnNum = 7,
        maxColumnNum = 7,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_FIELD_INFO
)
public class WriteDbHandler4FieldInfo extends AbstractWriteDbHandler<WriteDbData4FieldInfo> {

    public WriteDbHandler4FieldInfo(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4FieldInfo genData(String[] array) {
        String className = array[0];

        // 根据类名前缀判断是否需要处理
        if (!isAllowedClassPrefix(className)) {
            return null;
        }

        String simpleClassName = dbOperWrapper.getSimpleClassName(className);
        String fieldName = array[1];
        String fieldType = array[2];
        String modifiers = array[3];
        int primitiveType = Integer.parseInt(array[4]);
        int staticFlag = Integer.parseInt(array[5]);
        int finalFlag = Integer.parseInt(array[6]);
        WriteDbData4FieldInfo writeDbData4FieldInfo = new WriteDbData4FieldInfo();
        writeDbData4FieldInfo.setSimpleClassName(simpleClassName);
        writeDbData4FieldInfo.setFieldName(fieldName);
        writeDbData4FieldInfo.setFieldType(fieldType);
        writeDbData4FieldInfo.setModifiers(modifiers);
        writeDbData4FieldInfo.setPrimitiveType(primitiveType);
        writeDbData4FieldInfo.setStaticFlag(staticFlag);
        writeDbData4FieldInfo.setFinalFlag(finalFlag);
        writeDbData4FieldInfo.setClassName(className);
        return writeDbData4FieldInfo;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4FieldInfo data) {
        return new Object[]{
                genNextRecordId(),
                data.getSimpleClassName(),
                data.getFieldName(),
                data.getFieldType(),
                data.getModifiers(),
                data.getPrimitiveType(),
                data.getStaticFlag(),
                data.getFinalFlag(),
                data.getClassName()
        };
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "完整类名",
                "字段名称",
                "字段类型",
                "字段修饰符",
                "基本类型，1:是，0:否",
                "static标志，1:是，0:否",
                "final标志，1:是，0:否"
        };
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "字段的信息，包括字段名称、类型、修饰符等"
        };
    }
}
