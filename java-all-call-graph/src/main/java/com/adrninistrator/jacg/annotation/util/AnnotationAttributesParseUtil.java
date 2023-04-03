package com.adrninistrator.jacg.annotation.util;

import com.adrninistrator.jacg.common.enums.AnnotationAttributesTypeEnum;
import com.adrninistrator.jacg.dto.annotation_attribute.BaseAnnotationAttribute;
import com.adrninistrator.jacg.dto.annotation_attribute.EmptyAnnotationAttribute;
import com.adrninistrator.jacg.dto.annotation_attribute.InvalidAnnotationAttribute;
import com.adrninistrator.jacg.dto.annotation_attribute.ListMapAnnotationAttribute;
import com.adrninistrator.jacg.dto.annotation_attribute.ListStringAnnotationAttribute;
import com.adrninistrator.jacg.dto.annotation_attribute.MapAnnotationAttribute;
import com.adrninistrator.jacg.dto.annotation_attribute.StringAnnotationAttribute;
import com.adrninistrator.jacg.util.JACGJsonUtil;
import com.adrninistrator.javacg.util.JavaCGUtil;
import com.fasterxml.jackson.core.type.TypeReference;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2022/8/28
 * @description: 对数据库中保存的注解属性进行解析的工具类
 */
public class AnnotationAttributesParseUtil {
    private static final Logger logger = LoggerFactory.getLogger(AnnotationAttributesParseUtil.class);

    /**
     * 解析List+String格式的注解属性值
     *
     * @param attributeValue
     * @return
     */
    public static List<String> parseListStringAttribute(String attributeValue) {
        return JACGJsonUtil.getObjFromJsonStr(attributeValue, new TypeReference<List<String>>() {
        });
    }

    /**
     * 从数据库记录解析注解属性
     *
     * @param attributeType
     * @param attributeValue
     * @return
     */
    public static BaseAnnotationAttribute parseFromDb(String attributeType, String attributeValue) {
        if (AnnotationAttributesTypeEnum.AATE_STRING.getPrefix().equals(attributeType) ||
                AnnotationAttributesTypeEnum.AATE_STRING_BASE64.getPrefix().equals(attributeType)) {
            // AATE_STRING_BASE64在读取文件写入数据库时已解码，直接使用即可
            return new StringAnnotationAttribute(attributeValue);
        }
        if (AnnotationAttributesTypeEnum.AATE_MAP.getPrefix().equals(attributeType)) {
            Map<String, Object> attributeMap = JACGJsonUtil.getObjFromJsonStr(attributeValue, new TypeReference<Map<String, Object>>() {
            });
            return new MapAnnotationAttribute(attributeMap);
        }
        if (AnnotationAttributesTypeEnum.AATE_LIST_STRING.getPrefix().equals(attributeType)) {
            return new ListStringAnnotationAttribute(parseListStringAttribute(attributeValue));
        }
        if (AnnotationAttributesTypeEnum.AATE_LIST_MAP.getPrefix().equals(attributeType)) {
            List<Map<String, Object>> attributeList = JACGJsonUtil.getObjFromJsonStr(attributeValue, new TypeReference<List<Map<String, Object>>>() {
            });
            return new ListMapAnnotationAttribute(attributeList);
        }
        if (StringUtils.isBlank(attributeType)) {
            // 注解属性类型为空，返回空属性
            return EmptyAnnotationAttribute.getInstance();
        }

        logger.error("格式非法的注解属性 {}", attributeValue);
        return InvalidAnnotationAttribute.getInstance();
    }

    /**
     * 从文件记录解析注解属性
     *
     * @param attributeType
     * @param attributeValue
     * @return
     */
    public static String parseFromFile(String attributeType, String attributeValue) {
        if (AnnotationAttributesTypeEnum.AATE_STRING_BASE64.getPrefix().equals(attributeType)) {
            // AATE_STRING_BASE64类型数据需要进行解码
            return JavaCGUtil.base64Decode(attributeValue);
        }

        return attributeValue;
    }

    private AnnotationAttributesParseUtil() {
        throw new IllegalStateException("illegal");
    }
}
