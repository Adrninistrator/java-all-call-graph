package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.annotation.util.AnnotationAttributesParseUtil;
import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4FieldAnnotation;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.javacg2.common.enums.JavaCG2OutPutFileTypeEnum;

/**
 * @author adrninistrator
 * @date 2023/7/11
 * @description: 写入数据库，字段的注解
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCG2OutPutFileTypeEnum.OPFTE_FIELD_ANNOTATION,
        minColumnNum = JACGConstants.ANNOTATION_COLUMN_NUM_WITHOUT_ATTRIBUTE_3,
        maxColumnNum = JACGConstants.ANNOTATION_COLUMN_NUM_WITH_ATTRIBUTE_6,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_FIELD_ANNOTATION
)
public class WriteDbHandler4FieldAnnotation extends AbstractWriteDbHandler<WriteDbData4FieldAnnotation> {

    public WriteDbHandler4FieldAnnotation(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4FieldAnnotation genData(String[] array) {
        // 拆分时限制列数，最后一列注解属性中可能出现空格
        String className = array[0];
        String simpleClassName = dbOperWrapper.querySimpleClassName(className);
        String fieldName = array[1];
        String annotationName = array[2];
        // 假如当前行的注解信息无属性，注解属性名称设为空字符串
        String attributeName = "";
        String attributeType = null;
        String attributeValue = null;
        if (array.length > JACGConstants.ANNOTATION_COLUMN_NUM_WITHOUT_ATTRIBUTE_3) {
            // 当前行的注解信息有属性
            attributeName = array[3];
            attributeType = array[4];
            // 从文件记录解析注解属性
            attributeValue = AnnotationAttributesParseUtil.parseFromFile(attributeType, array[5]);
        }

        WriteDbData4FieldAnnotation writeDbData4FieldAnnotation = new WriteDbData4FieldAnnotation();
        writeDbData4FieldAnnotation.setRecordId(genNextRecordId());
        writeDbData4FieldAnnotation.setSimpleClassName(simpleClassName);
        writeDbData4FieldAnnotation.setFieldName(fieldName);
        writeDbData4FieldAnnotation.setAnnotationName(annotationName);
        writeDbData4FieldAnnotation.setAttributeName(attributeName);
        writeDbData4FieldAnnotation.setAnnotationType(attributeType);
        writeDbData4FieldAnnotation.setAttributeValue(attributeValue);
        writeDbData4FieldAnnotation.setClassName(className);
        return writeDbData4FieldAnnotation;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4FieldAnnotation data) {
        return new Object[]{
                data.getRecordId(),
                data.getSimpleClassName(),
                data.getFieldName(),
                data.getAnnotationName(),
                data.getAttributeName(),
                data.getAnnotationType(),
                data.getAttributeValue(),
                data.getClassName()
        };
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "完整类名",
                "字段名称",
                "注解类名",
                "注解属性名称，空字符串代表无注解属性",
                "注解属性类型，参考AnnotationAttributesTypeEnum类",
                "注解属性值"
        };
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "字段上指定的注解信息",
                "若注解没有属性值，则相关字段为空",
                "若注解有属性值，则每个属性值占一行"
        };
    }
}
