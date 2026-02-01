package com.adrninistrator.jacg.handler.dto;

import com.adrninistrator.jacg.dto.writedb.WriteDbData4PropertiesConf;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2026/1/27
 * @description: XML配置信息，包含从properties文件获取的值
 */
public class XmlConfWithProperties {
    /**
     * 原始的元素属性值
     */
    private String originalValue;

    /**
     * 元素属性值对应的默认值（当属性值为${a:b}格式时，b为默认值）
     */
    private String defaultValue;

    /**
     * 从properties文件查询到的记录列表
     */
    private List<WriteDbData4PropertiesConf> propertiesConfList;

    public String getOriginalValue() {
        return originalValue;
    }

    public void setOriginalValue(String originalValue) {
        this.originalValue = originalValue;
    }

    public String getDefaultValue() {
        return defaultValue;
    }

    public void setDefaultValue(String defaultValue) {
        this.defaultValue = defaultValue;
    }

    public List<WriteDbData4PropertiesConf> getPropertiesConfList() {
        return propertiesConfList;
    }

    public void setPropertiesConfList(List<WriteDbData4PropertiesConf> propertiesConfList) {
        this.propertiesConfList = propertiesConfList;
    }
}
