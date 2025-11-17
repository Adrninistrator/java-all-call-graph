package com.adrninistrator.jacg.handler.dto.string;

import com.adrninistrator.jacg.handler.dto.string.element.BaseStringElement;

import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2025/9/19
 * @description: 字符串拼接解析结果，方法内部处理使用
 */
public class StringAppendParseResultInner {

    // 原始字符串，对于枚举常量方法调用返回值，使用 {简单类名}.{枚举常量}.{被调用方法}() 形式。存在多种可能时，代表第一种可能的值
    private String rawString;

    // 解析后的字符串，对于枚举常量方法调用返回值，使用对应方法返回的常量值。存在多种可能时，代表第一种可能的值
    private String parsedValue;

    // 字符串元素列表，存在多种可能性
    private final List<BaseStringElement> stringElementList = new ArrayList<>();

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
}
