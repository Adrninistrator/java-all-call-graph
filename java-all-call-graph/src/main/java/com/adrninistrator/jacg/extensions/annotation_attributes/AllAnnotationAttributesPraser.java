package com.adrninistrator.jacg.extensions.annotation_attributes;

import com.adrninistrator.jacg.common.enums.AnnotationAttributesTypeEnum;
import com.adrninistrator.jacg.dto.annotation_attribute.BaseAnnotationAttribute;
import com.adrninistrator.jacg.dto.annotation_attribute.InvalidAnnotationAttribute;
import com.adrninistrator.jacg.dto.annotation_attribute.ListMapAnnotationAttribute;
import com.adrninistrator.jacg.dto.annotation_attribute.ListStringAnnotationAttribute;
import com.adrninistrator.jacg.dto.annotation_attribute.MapAnnotationAttribute;
import com.adrninistrator.jacg.dto.annotation_attribute.StringAnnotationAttribute;
import com.adrninistrator.jacg.extensions.util.JsonUtil;
import com.fasterxml.jackson.core.type.TypeReference;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2022/8/28
 * @description: 对数据库中保存的注解属性进行解析
 */
public class AllAnnotationAttributesPraser {
    private static final Logger logger = LoggerFactory.getLogger(AllAnnotationAttributesPraser.class);

    // 解析注解属性
    public static BaseAnnotationAttribute parse(String data) {
        if (StringUtils.startsWith(data, AnnotationAttributesTypeEnum.AATE_STRING.getPrefix())) {
            return handleString(data, AnnotationAttributesTypeEnum.AATE_STRING.getPrefixLength());
        }

        if (StringUtils.startsWith(data, AnnotationAttributesTypeEnum.AATE_STRING_BASE64.getPrefix())) {
            return handleStringBase64(data, AnnotationAttributesTypeEnum.AATE_STRING_BASE64.getPrefixLength());
        }

        if (StringUtils.startsWith(data, AnnotationAttributesTypeEnum.AATE_MAP.getPrefix())) {
            return handleMap(data, AnnotationAttributesTypeEnum.AATE_MAP.getPrefixLength());
        }

        if (StringUtils.startsWith(data, AnnotationAttributesTypeEnum.AATE_LIST_STRING.getPrefix())) {
            return handleListString(data, AnnotationAttributesTypeEnum.AATE_LIST_STRING.getPrefixLength());
        }

        if (StringUtils.startsWith(data, AnnotationAttributesTypeEnum.AATE_LIST_MAP.getPrefix())) {
            return handleListMap(data, AnnotationAttributesTypeEnum.AATE_LIST_MAP.getPrefixLength());
        }

        logger.error("格式非法的注解属性 {}", data);
        return InvalidAnnotationAttribute.getInstance();
    }

    private static StringAnnotationAttribute handleString(String data, int prefixLength) {
        String dataWithOutPrefix = data.substring(prefixLength);
        return new StringAnnotationAttribute(dataWithOutPrefix);
    }

    private static StringAnnotationAttribute handleStringBase64(String data, int prefixLength) {
        String dataWithOutPrefix = data.substring(prefixLength);
        return new StringAnnotationAttribute(new String(Base64.getDecoder().decode(dataWithOutPrefix), StandardCharsets.UTF_8));
    }

    private static MapAnnotationAttribute handleMap(String data, int prefixLength) {
        String dataWithOutPrefix = data.substring(prefixLength);
        Map<String, Object> attributeMap = JsonUtil.getObjFromJsonStr(dataWithOutPrefix, new TypeReference<Map<String, Object>>() {
        });
        return new MapAnnotationAttribute(attributeMap);
    }

    private static ListStringAnnotationAttribute handleListString(String data, int prefixLength) {
        String dataWithOutPrefix = data.substring(prefixLength);
        List<String> attributeList = JsonUtil.getObjFromJsonStr(dataWithOutPrefix, new TypeReference<List<String>>() {
        });
        return new ListStringAnnotationAttribute(attributeList);
    }

    private static ListMapAnnotationAttribute handleListMap(String data, int prefixLength) {
        String dataWithOutPrefix = data.substring(prefixLength);
        List<Map<String, Object>> attributeList = JsonUtil.getObjFromJsonStr(dataWithOutPrefix, new TypeReference<List<Map<String, Object>>>() {
        });
        return new ListMapAnnotationAttribute(attributeList);
    }
}
