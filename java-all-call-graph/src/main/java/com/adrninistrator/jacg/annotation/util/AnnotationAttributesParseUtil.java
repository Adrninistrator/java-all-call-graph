package com.adrninistrator.jacg.annotation.util;

import com.adrninistrator.jacg.common.enums.AnnotationAttributesTypeEnum;
import com.adrninistrator.jacg.dto.annotation.AnnotationAttributeInfo;
import com.adrninistrator.jacg.dto.annotation.BaseAnnotationAttribute;
import com.adrninistrator.jacg.dto.annotation.EmptyAnnotationAttribute;
import com.adrninistrator.jacg.dto.annotation.InvalidAnnotationAttribute;
import com.adrninistrator.jacg.dto.annotation.ListMapAnnotationAttribute;
import com.adrninistrator.jacg.dto.annotation.ListStringAnnotationAttribute;
import com.adrninistrator.jacg.dto.annotation.MapAnnotationAttribute;
import com.adrninistrator.jacg.dto.annotation.StringAnnotationAttribute;
import com.adrninistrator.jacg.extractor.common.enums.SpTxPropagationEnum;
import com.adrninistrator.jacg.util.JACGJsonUtil;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import com.fasterxml.jackson.core.type.TypeReference;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2022/8/28
 * @description: 对注解属性进行解析的工具类
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
     * 根据从数据库查询的注解属性信息获取对应的注解属性值
     *
     * @param annotationAttributeInfo
     * @return
     */
    public static BaseAnnotationAttribute genAnnotationAttribute(AnnotationAttributeInfo annotationAttributeInfo) {
        // 解析注解属性
        return parseFromDb(annotationAttributeInfo.getAttributeType(), annotationAttributeInfo.getAttributeValue());
    }

    /**
     * 从数据库记录解析注解属性
     *
     * @param attributeType
     * @param attributeValue
     * @return
     */
    public static BaseAnnotationAttribute parseFromDb(String attributeType, String attributeValue) {
        if (StringUtils.isBlank(attributeType)) {
            // 注解属性类型为空，返回空属性
            return EmptyAnnotationAttribute.getInstance();
        }
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
            return JavaCG2Util.base64Decode(attributeValue);
        }

        return attributeValue;
    }

    /**
     * 从注解属性Map中，根据注解属性名，获取预期类型的注解属性值
     *
     * @param annotationAttributeMap 注解属性Map，key：注解属性名称，value：注解属性
     * @param attributeName          注解属性名
     * @return attributeClassType 预期的注解属性类型
     * @return
     */
    @SuppressWarnings("unchecked")
    public static <T extends BaseAnnotationAttribute> T getAttributeValueFromMap(Map<String, BaseAnnotationAttribute> annotationAttributeMap,
                                                                                 String attributeName,
                                                                                 Class<T> attributeClassType) {
        if (annotationAttributeMap == null || attributeName == null || attributeClassType == null) {
            logger.error("传入参数为空 {} {} {}", annotationAttributeMap, attributeName, attributeClassType);
            return null;
        }

        BaseAnnotationAttribute attribute = annotationAttributeMap.get(attributeName);
        if (attribute == null) {
            logger.debug("注解属性为空 {}", attributeName);
            return null;
        }

        if (!attributeClassType.isAssignableFrom(attribute.getClass())) {
            logger.error("类注解属性的实现类型与预期不一致 {}\n{}\n{}", attributeName, attribute.getClass().getName(), attributeClassType.getName());
            return null;
        }
        return (T) attribute;
    }

    /**
     * 从注解属性Map中获取类型为String的属性值
     *
     * @param annotationAttributeMap 注解属性Map
     * @param attributeName          注解属性名称
     * @return
     */
    public static String getAttributeStringValue(Map<String, BaseAnnotationAttribute> annotationAttributeMap, String attributeName) {
        StringAnnotationAttribute stringAnnotationAttribute = getAttributeValueFromMap(annotationAttributeMap, attributeName, StringAnnotationAttribute.class);
        if (stringAnnotationAttribute == null) {
            return null;
        }
        return stringAnnotationAttribute.getAttributeString();
    }

    /**
     * 获取Spring事务注解@Transactional对应的事务传播行为，仅当确认对应方法上有@Transactional注解时，才能使用当前方法查询
     *
     * @param txPropagationAttribute
     * @return
     */
    public static String getSpringTxAnnotationPropagation(BaseAnnotationAttribute txPropagationAttribute) {
        if (txPropagationAttribute == null) {
            return SpTxPropagationEnum.STPE_DEFAULT_REQUIRED.getPropagation();
        }
        return ((StringAnnotationAttribute) txPropagationAttribute).getAttributeString();
    }

    private AnnotationAttributesParseUtil() {
        throw new IllegalStateException("illegal");
    }
}
