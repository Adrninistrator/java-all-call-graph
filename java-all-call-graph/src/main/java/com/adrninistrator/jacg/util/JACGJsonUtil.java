package com.adrninistrator.jacg.util;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ObjectWriter;
import com.fasterxml.jackson.databind.SerializationFeature;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Map;

public class JACGJsonUtil {
    private static final Logger logger = LoggerFactory.getLogger(JACGJsonUtil.class);

    private static final ObjectMapper objectMapper = new ObjectMapper();
    private static final ObjectWriter objectWriterPretty;

    static {
        JsonInclude.Value dense = JsonInclude.Value.construct(JsonInclude.Include.NON_NULL, JsonInclude.Include.NON_NULL);
        objectMapper.setDefaultPropertyInclusion(dense);

        objectMapper.disable(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES);
        objectMapper.disable(SerializationFeature.FAIL_ON_EMPTY_BEANS);
        // 序列化前Map按Key排序，使不同情况下对Map序列化后的顺序是一致的
        objectMapper.enable(SerializationFeature.ORDER_MAP_ENTRIES_BY_KEYS);
        objectMapper.enable(JsonGenerator.Feature.WRITE_BIGDECIMAL_AS_PLAIN);

        objectWriterPretty = objectMapper.writerWithDefaultPrettyPrinter();
    }

    public static String getJsonStr(Object obj) {
        try {
            return objectMapper.writeValueAsString(obj);
        } catch (Exception e) {
            logger.error("error ", e);
            return null;
        }
    }

    public static String getJsonStrPretty(Object obj) {
        if (obj instanceof String) {
            return (String) obj;
        }

        try {
            return objectWriterPretty.writeValueAsString(obj);
        } catch (Exception e) {
            logger.error("error ", e);
            return null;
        }
    }

    public static Map<String, Object> getMapFromJsonStr(String jsonStr) {
        try {
            return objectMapper.readValue(jsonStr, new TypeReference<Map<String, Object>>() {
            });
        } catch (Exception e) {
            logger.error("error ", e);
            return null;
        }
    }

    public static <T> T getObjFromJsonStr(String jsonStr, Class<T> clazz) {
        try {
            return objectMapper.readValue(jsonStr, clazz);
        } catch (Exception e) {
            logger.error("error ", e);
            return null;
        }
    }

    public static <T> T getObjFromJsonStr(String jsonStr, TypeReference<T> clazz) {
        try {
            return objectMapper.readValue(jsonStr, clazz);
        } catch (Exception e) {
            logger.error("error ", e);
            return null;
        }
    }

    private JACGJsonUtil() {
        throw new IllegalStateException("illegal");
    }
}
