package com.adrninistrator.jacg.handler.write_db;

import com.adrninistrator.jacg.annotation.util.AnnotationAttributesParseUtil;
import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.write_db.WriteDbData4ClassAnnotation;
import com.adrninistrator.jacg.util.spring.SpringMvcRequestMappingUtil;
import com.adrninistrator.javacg.common.enums.JavaCGOutPutFileTypeEnum;
import com.adrninistrator.javacg.dto.output.JavaCGOutputInfo;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;


/**
 * @author adrninistrator
 * @date 2022/11/15
 * @description: 写入数据库，类的注解
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCGOutPutFileTypeEnum.OPFTE_CLASS_ANNOTATION,
        minColumnNum = JACGConstants.ANNOTATION_COLUMN_NUM_WITHOUT_ATTRIBUTE,
        maxColumnNum = JACGConstants.ANNOTATION_COLUMN_NUM_WITH_ATTRIBUTE,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_CLASS_ANNOTATION
)
public class WriteDbHandler4ClassAnnotation extends AbstractWriteDbHandler<WriteDbData4ClassAnnotation> {
    /*
        保存Spring MVC相关类名及@RequestMapping注解属性值
        key
            Spring MVC相关类的唯一类名
        value
            @RequestMapping注解属性值列表
     */
    private final Map<String, List<String>> classRequestMappingMap = new HashMap<>();

    public WriteDbHandler4ClassAnnotation(JavaCGOutputInfo javaCGOutputInfo) {
        super(javaCGOutputInfo);
    }

    @Override
    protected WriteDbData4ClassAnnotation genData(String[] array) {
        // 拆分时限制列数，最后一列注解属性中可能出现空格
        String className = array[0];

        // 根据类名前缀判断是否需要处理
        if (!isAllowedClassPrefix(className)) {
            return null;
        }

        String simpleClassName = dbOperWrapper.getSimpleClassName(className);
        String annotationName = array[1];
        // 假如当前行的注解信息无属性，注解属性名称设为空字符串
        String attributeName = "";
        String attributeType = null;
        String attributeValue = null;
        if (array.length > JACGConstants.ANNOTATION_COLUMN_NUM_WITHOUT_ATTRIBUTE) {
            // 当前行的注解信息有属性
            attributeName = array[2];
            attributeType = array[3];
            // 从文件记录解析注解属性
            attributeValue = AnnotationAttributesParseUtil.parseFromFile(attributeType, array[4]);
        }

        if (SpringMvcRequestMappingUtil.isRequestMappingAnnotation(annotationName)) {
            if (attributeName.isEmpty()) {
                // 类上的Spring MVC对应的@RequestMapping注解的path属性值为空
                classRequestMappingMap.put(simpleClassName, Collections.emptyList());
            } else if (SpringMvcRequestMappingUtil.isRequestMappingPathAttribute(attributeName)) {
                // 处理类上的Spring MVC对应的@RequestMapping注解的path属性值
                classRequestMappingMap.put(simpleClassName, AnnotationAttributesParseUtil.parseListStringAttribute(attributeValue));
            }
        }

        return new WriteDbData4ClassAnnotation(simpleClassName, annotationName, attributeName, attributeType, attributeValue, className);
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4ClassAnnotation data) {
        return new Object[]{
                genNextRecordId(),
                data.getSimpleClassName(),
                data.getAnnotationName(),
                data.getAttributeName(),
                data.getAnnotationType(),
                data.getAttributeValue(),
                data.getClassName()
        };
    }

    public Map<String, List<String>> getClassRequestMappingMap() {
        return classRequestMappingMap;
    }
}
