package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.annotation.util.AnnotationAttributesParseUtil;
import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodArgAnnotation;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg.common.enums.JavaCGOutPutFileTypeEnum;

/**
 * @author adrninistrator
 * @date 2023/11/5
 * @description: 写入数据库，方法参数的注解
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCGOutPutFileTypeEnum.OPFTE_METHOD_ARG_ANNOTATION,
        minColumnNum = JACGConstants.ANNOTATION_COLUMN_NUM_WITHOUT_ATTRIBUTE_3,
        maxColumnNum = JACGConstants.ANNOTATION_COLUMN_NUM_WITH_ATTRIBUTE_6,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_METHOD_ARG_ANNOTATION
)
public class WriteDbHandler4MethodArgAnnotation extends AbstractWriteDbHandler<WriteDbData4MethodArgAnnotation> {

    public WriteDbHandler4MethodArgAnnotation(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4MethodArgAnnotation genData(String[] array) {
        // 拆分时限制列数，最后一列注解属性中可能出现空格
        String fullMethod = array[0];
        // 根据完整方法前缀判断是否需要处理
        if (!isAllowedClassPrefix(fullMethod)) {
            return null;
        }

        String className = JACGClassMethodUtil.getClassNameFromMethod(fullMethod);
        String simpleClassName = dbOperWrapper.getSimpleClassName(className);
        String methodHash = JACGUtil.genHashWithLen(fullMethod);
        int argSeq = Integer.parseInt(array[1]);
        String annotationName = array[2];
        // 若当前行的注解信息无属性，注解属性名称设为空字符串
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

        WriteDbData4MethodArgAnnotation WriteDbData4MethodArgAnnotation = new WriteDbData4MethodArgAnnotation();
        WriteDbData4MethodArgAnnotation.setMethodHash(methodHash);
        WriteDbData4MethodArgAnnotation.setArgSeq(argSeq);
        WriteDbData4MethodArgAnnotation.setAnnotationName(annotationName);
        WriteDbData4MethodArgAnnotation.setAttributeName(attributeName);
        WriteDbData4MethodArgAnnotation.setAnnotationType(attributeType);
        WriteDbData4MethodArgAnnotation.setAttributeValue(attributeValue);
        WriteDbData4MethodArgAnnotation.setFullMethod(fullMethod);
        WriteDbData4MethodArgAnnotation.setSimpleClassName(simpleClassName);
        return WriteDbData4MethodArgAnnotation;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4MethodArgAnnotation data) {
        return new Object[]{
                genNextRecordId(),
                data.getMethodHash(),
                data.getArgSeq(),
                data.getAnnotationName(),
                data.getAttributeName(),
                data.getAnnotationType(),
                data.getAttributeValue(),
                data.getFullMethod(),
                data.getSimpleClassName(),
        };
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "完整方法（类名+方法名+参数）",
                "参数序号，从0开始",
                "注解类名",
                "注解属性名称，空字符串代表无注解属性",
                "注解属性类型，参考AnnotationAttributesTypeEnum类",
                "注解属性值"
        };
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "方法参数上指定的注解信息",
                "若注解没有属性值，则相关字段为空",
                "若注解有属性值，则每个属性值占一行"
        };
    }
}
