package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4FieldGenericsType;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.javacg2.common.enums.JavaCG2OutPutFileTypeEnum;

/**
 * @author adrninistrator
 * @date 2023/8/9
 * @description: 写入数据库，非静态字段中涉及的泛型类型
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCG2OutPutFileTypeEnum.OPFTE_FIELD_GENERICS_TYPE,
        minColumnNum = 10,
        maxColumnNum = 10,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_FIELD_GENERICS_TYPE
)
public class WriteDbHandler4FieldGenericsType extends AbstractWriteDbHandler<WriteDbData4FieldGenericsType> {

    public WriteDbHandler4FieldGenericsType(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4FieldGenericsType genData(String[] array) {
        String className = readLineData();
        String simpleClassName = dbOperWrapper.querySimpleClassName(className);
        String fieldName = readLineData();
        String type = readLineData();
        int typeSeq = Integer.parseInt(readLineData());
        String genericsType = readLineData();
        String simpleGenericsType = dbOperWrapper.querySimpleClassName(genericsType);
        int genericsArrayDimensions = Integer.parseInt(readLineData());
        String typeVariablesName = readLineData();
        String wildcard = readLineData();
        String referenceType = readLineData();
        String genericsCategory = readLineData();

        WriteDbData4FieldGenericsType writeDbData4FieldGenericsType = new WriteDbData4FieldGenericsType();
        writeDbData4FieldGenericsType.setRecordId(genNextRecordId());
        writeDbData4FieldGenericsType.setSimpleClassName(simpleClassName);
        writeDbData4FieldGenericsType.setFieldName(fieldName);
        writeDbData4FieldGenericsType.setType(type);
        writeDbData4FieldGenericsType.setTypeSeq(typeSeq);
        writeDbData4FieldGenericsType.setSimpleGenericsType(simpleGenericsType);
        writeDbData4FieldGenericsType.setGenericsArrayDimensions(genericsArrayDimensions);
        writeDbData4FieldGenericsType.setTypeVariablesName(typeVariablesName);
        writeDbData4FieldGenericsType.setWildcard(wildcard);
        writeDbData4FieldGenericsType.setReferenceType(referenceType);
        writeDbData4FieldGenericsType.setGenericsCategory(genericsCategory);
        writeDbData4FieldGenericsType.setGenericsType(genericsType);
        writeDbData4FieldGenericsType.setClassName(className);
        return writeDbData4FieldGenericsType;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4FieldGenericsType data) {
        return new Object[]{
                data.getRecordId(),
                data.getSimpleClassName(),
                data.getFieldName(),
                data.getType(),
                data.getTypeSeq(),
                data.getSimpleGenericsType(),
                data.getGenericsArrayDimensions(),
                data.getTypeVariablesName(),
                data.getWildcard(),
                data.getReferenceType(),
                data.getGenericsCategory(),
                data.getGenericsType(),
                data.getClassName()
        };
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "完整类名",
                "字段名",
                "类型，t:字段类型，gt:字段中的泛型类型",
                "类型序号，字段类型固定为0，字段的泛型类型从0开始",
                "非静态字段类型或其中的泛型类型类名",
                "非静态字段中的泛型数组类型的维度，为0代表不是数组类型",
                "非静态字段中的泛型类型变量名称",
                "非静态字段中的泛型通配符",
                "非静态字段中的泛型通配符引用的类型",
                "非静态字段中的泛型类型分类，J:JDK中的类型，C:自定义类型"
        };
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "非静态字段中涉及的泛型类型，每个字段中可能涉及多种泛型类型，可能会存在多条记录"
        };
    }
}
