package com.adrninistrator.jacg.handler.dto.string;

import com.adrninistrator.jacg.handler.dto.string.element.BaseStringElement;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2025/9/2
 * @description: 字符串拼接解析结果
 */
public class StringAppendParseResult {

    // 原始字符串，对于枚举常量方法调用返回值，使用 {简单类名}.{枚举常量}.{被调用方法}() 形式
    private String rawString;

    // 解析后的字符串，对于枚举常量方法调用返回值，使用对应方法返回的常量值
    private String parsedValue;

    // 字符串元素列表
    private List<BaseStringElement> stringElementList;

    public String getRawString() {
        return rawString;
    }

    public void setRawString(String rawString) {
        this.rawString = rawString;
    }

    public String getParsedValue() {
        return parsedValue;
    }

    public void setParsedValue(String parsedValue) {
        this.parsedValue = parsedValue;
    }

    public List<BaseStringElement> getStringElementList() {
        return stringElementList;
    }

    public void setStringElementList(List<BaseStringElement> stringElementList) {
        this.stringElementList = stringElementList;
    }
}
