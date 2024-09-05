package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4FieldGenericsType;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.javacg2.common.enums.JavaCG2OutPutFileTypeEnum;

/**
 * @author adrninistrator
 * @date 2023/8/9
 * @description: 写入数据库，dto的非静态字段集合中涉及的泛型类型
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCG2OutPutFileTypeEnum.OPFTE_FIELD_GENERICS_TYPE,
        minColumnNum = 5,
        maxColumnNum = 5,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_FIELD_GENERICS_TYPE
)
public class WriteDbHandler4FieldGenericsType extends AbstractWriteDbHandler<WriteDbData4FieldGenericsType> {

    public WriteDbHandler4FieldGenericsType(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4FieldGenericsType genData(String[] array) {
        String className = array[0];
        // 根据类名前缀判断是否需要处理
        if (!isAllowedClassPrefix(className)) {
            return null;
        }

        String simpleClassName = dbOperWrapper.querySimpleClassName(className);
        String fieldName = array[1];
        int seq = Integer.parseInt(array[2]);
        String fieldCategory = array[3];
        String fieldGenericsType = array[4];
        String simpleFieldGenericsType = dbOperWrapper.querySimpleClassName(fieldGenericsType);

        WriteDbData4FieldGenericsType writeDbData4FieldGenericsType = new WriteDbData4FieldGenericsType();
        writeDbData4FieldGenericsType.setRecordId(genNextRecordId());
        writeDbData4FieldGenericsType.setSimpleClassName(simpleClassName);
        writeDbData4FieldGenericsType.setFieldName(fieldName);
        writeDbData4FieldGenericsType.setSeq(seq);
        writeDbData4FieldGenericsType.setFieldCategory(fieldCategory);
        writeDbData4FieldGenericsType.setSimpleFieldGenericsType(simpleFieldGenericsType);
        writeDbData4FieldGenericsType.setFieldGenericsType(fieldGenericsType);
        writeDbData4FieldGenericsType.setClassName(className);
        return writeDbData4FieldGenericsType;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4FieldGenericsType data) {
        return new Object[]{
                data.getRecordId(),
                data.getSimpleClassName(),
                data.getFieldName(),
                data.getSeq(),
                data.getFieldCategory(),
                data.getSimpleFieldGenericsType(),
                data.getFieldGenericsType(),
                data.getClassName()
        };
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "完整类名",
                "字段名",
                "字段集合中的泛型类型序号，从0开始",
                "字段集合中的泛型类型分类，J:JDK中的类型，C:自定义类型",
                "字段集合中的泛型类型"
        };
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "dto的非静态字段集合中涉及的泛型类型，每个字段的集合中可能涉及多种泛型类型，会存在多条记录"
        };
    }
}
