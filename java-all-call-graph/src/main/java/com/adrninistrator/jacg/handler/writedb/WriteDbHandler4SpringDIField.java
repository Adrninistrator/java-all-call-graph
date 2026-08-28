package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4SpringDIField;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.javacg2.common.enums.JavaCG2OutPutFileTypeEnum;

/**
 * @author adrninistrator
 * @date 2026/6/19
 * @description: 写入数据库，Spring依赖注入字段信息
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCG2OutPutFileTypeEnum.OPFTE_SPRING_DI_FIELD,
        minColumnNum = 4,
        maxColumnNum = 4,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_SPRING_DI_FIELD
)
public class WriteDbHandler4SpringDIField extends AbstractWriteDbHandler<WriteDbData4SpringDIField> {

    public WriteDbHandler4SpringDIField(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4SpringDIField genData(String[] array) {
        // 读取中间文件的每行数据，格式：className\tfieldType\tfieldName\tbeanType
        String className = readLineData();
        String fieldType = readLineData();
        String fieldName = readLineData();
        String beanType = readLineData();

        WriteDbData4SpringDIField writeDbData4SpringDIField = new WriteDbData4SpringDIField();
        writeDbData4SpringDIField.setRecordId(genNextRecordId());
        writeDbData4SpringDIField.setClassName(className);
        writeDbData4SpringDIField.setSimpleClassName(dbOperWrapper.querySimpleClassName(className));
        writeDbData4SpringDIField.setFieldType(fieldType);
        writeDbData4SpringDIField.setSimpleFieldType(dbOperWrapper.querySimpleClassName(fieldType));
        writeDbData4SpringDIField.setFieldName(fieldName);
        writeDbData4SpringDIField.setBeanType(beanType);
        writeDbData4SpringDIField.setSimpleBeanType(dbOperWrapper.querySimpleClassName(beanType));
        // 判断字段声明类型与Spring Bean实际注入类型是否相同
        writeDbData4SpringDIField.setSameType(fieldType.equals(beanType) ? 1 : 0);
        return writeDbData4SpringDIField;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4SpringDIField data) {
        return new Object[]{
                data.getRecordId(),
                data.getClassName(),
                data.getSimpleClassName(),
                data.getFieldType(),
                data.getSimpleFieldType(),
                data.getFieldName(),
                data.getBeanType(),
                data.getSimpleBeanType(),
                data.getSameType()
        };
    }

    @Override
    public String[] chooseFileColumnDesc() {
        // 只描述中间文件中的列（4列），simple_class_name和simple_bean_type是从className和beanType推导生成的，不在文件中
        return new String[]{
                "完整类名",
                "字段声明类型（接口类型）",
                "字段名",
                "Spring Bean实际类型"
        };
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "Spring依赖注入字段信息，包括字段的声明类型与Spring Bean实际类型"
        };
    }
}
