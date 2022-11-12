package com.adrninistrator.jacg.extensions.annotation_attributes;

import com.adrninistrator.jacg.common.enums.AnnotationAttributesTypeEnum;
import com.adrninistrator.jacg.extensions.util.JsonUtil;
import com.adrninistrator.javacg.extensions.annotation_attributes.AnnotationAttributesFormatorInterface;
import org.apache.bcel.classfile.AnnotationElementValue;
import org.apache.bcel.classfile.AnnotationEntry;
import org.apache.bcel.classfile.ArrayElementValue;
import org.apache.bcel.classfile.ClassElementValue;
import org.apache.bcel.classfile.ElementValue;
import org.apache.bcel.classfile.ElementValuePair;
import org.apache.bcel.classfile.EnumElementValue;
import org.apache.bcel.classfile.SimpleElementValue;
import org.apache.bcel.classfile.Utility;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Base64;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2022/8/28
 * @description:
 */
public class AllAnnotationAttributesFormator implements AnnotationAttributesFormatorInterface {
    private static final Logger logger = LoggerFactory.getLogger(AllAnnotationAttributesFormator.class);

    @Override
    public String format(ElementValuePair elementValuePair) {
        String name = elementValuePair.getNameString();
        String valueString = null;

        // 获取注解属性
        Pair<Object, AnnotationAttributesTypeEnum> pair = getElementValue(elementValuePair.getValue());
        Object value = pair.getLeft();
        AnnotationAttributesTypeEnum annotationAttributesTypeEnum = pair.getRight();

        if (value instanceof String) {
            // 注解属性为String类型，直接使用String
            valueString = handleStringValue((String) value);
        } else if (value instanceof Map) {
            // 注解属性为Map类型，进行JSON序列化
            valueString = annotationAttributesTypeEnum.getPrefix() + JsonUtil.getJsonStr(value);
        } else if (value instanceof List) {
            // 注解属性为List类型，进行JSON序列化
            valueString = annotationAttributesTypeEnum.getPrefix() + JsonUtil.getJsonStr(value);
        }

        // TODO 后面把空格改成\t
        return name + " " + valueString;
    }

    private String handleStringValue(String value) {
        if (StringUtils.containsAny(value, "\r", "\n")) {
            // 若字符串内容包含回车换行，则需要进行BASE64编码，避免写到文件后导致换行
            return AnnotationAttributesTypeEnum.AATE_STRING_BASE64.getPrefix() + Base64.getEncoder().encodeToString(value.getBytes(StandardCharsets.UTF_8));
        }

        // 若字符串内容不包含回车换行，则使用原始值
        return AnnotationAttributesTypeEnum.AATE_STRING.getPrefix() + value;
    }

    // 获取注解属性
    private Pair<Object, AnnotationAttributesTypeEnum> getElementValue(ElementValue elementValue) {
        if (elementValue instanceof SimpleElementValue) {
            return getSimpleElementValue((SimpleElementValue) elementValue);
        }

        if (elementValue instanceof ClassElementValue) {
            return getClassElementValue((ClassElementValue) elementValue);
        }

        if (elementValue instanceof EnumElementValue) {
            return getEnumElementValue((EnumElementValue) elementValue);
        }

        if (elementValue instanceof AnnotationElementValue) {
            return getAnnotationElementValue((AnnotationElementValue) elementValue);
        }

        if (elementValue instanceof ArrayElementValue) {
            return getArrayElementValue((ArrayElementValue) elementValue);
        }

        logger.error("不支持的类型 {}", elementValue.getClass().getName());
        return new ImmutablePair<>(new Object(), AnnotationAttributesTypeEnum.AATE_NOT_SUPPORT);
    }

    private Pair<Object, AnnotationAttributesTypeEnum> getSimpleElementValue(SimpleElementValue simpleElementValue) {
        return new ImmutablePair<>(simpleElementValue.toString(), AnnotationAttributesTypeEnum.AATE_STRING);
    }

    private Pair<Object, AnnotationAttributesTypeEnum> getClassElementValue(ClassElementValue classElementValue) {
        String className = Utility.typeSignatureToString(classElementValue.getClassString(), false);
        return new ImmutablePair<>(className, AnnotationAttributesTypeEnum.AATE_STRING);
    }

    private Pair<Object, AnnotationAttributesTypeEnum> getEnumElementValue(EnumElementValue enumElementValue) {
        return new ImmutablePair<>(enumElementValue.getEnumValueString(), AnnotationAttributesTypeEnum.AATE_STRING);
    }

    private Pair<Object, AnnotationAttributesTypeEnum> getAnnotationElementValue(AnnotationElementValue annotationElementValue) {
        AnnotationEntry annotationEntry = annotationElementValue.getAnnotationEntry();
        if (annotationEntry.getElementValuePairs() == null || annotationEntry.getElementValuePairs().length == 0) {
            // 注解属性为空
            return new ImmutablePair<>(new HashMap<>(), AnnotationAttributesTypeEnum.AATE_MAP);
        }

        Map<String, Object> attributesMap = new HashMap<>(annotationEntry.getElementValuePairs().length);
        for (ElementValuePair elementValuePair : annotationEntry.getElementValuePairs()) {
            String name = elementValuePair.getNameString();
            // 获取注解属性
            Pair<Object, AnnotationAttributesTypeEnum> pair = getElementValue(elementValuePair.getValue());
            Object value = pair.getLeft();
            attributesMap.put(name, value);
        }

        return new ImmutablePair<>(attributesMap, AnnotationAttributesTypeEnum.AATE_MAP);
    }

    private Pair<Object, AnnotationAttributesTypeEnum> getArrayElementValue(ArrayElementValue arrayElementValue) {
        AnnotationAttributesTypeEnum listElementTypeEnum = null;

        List<Object> valueList = new ArrayList<>(arrayElementValue.getElementValuesArray().length);
        for (ElementValue elementValue : arrayElementValue.getElementValuesArray()) {
            // 获取注解属性
            Pair<Object, AnnotationAttributesTypeEnum> pair = getElementValue(elementValue);
            valueList.add(pair.getLeft());

            if (listElementTypeEnum == null) {
                listElementTypeEnum = pair.getRight();
            }
        }

        AnnotationAttributesTypeEnum listTypeEnum = (listElementTypeEnum == AnnotationAttributesTypeEnum.AATE_STRING ?
                AnnotationAttributesTypeEnum.AATE_LIST_STRING :
                AnnotationAttributesTypeEnum.AATE_LIST_MAP);

        return new ImmutablePair<>(valueList, listTypeEnum);
    }
}
