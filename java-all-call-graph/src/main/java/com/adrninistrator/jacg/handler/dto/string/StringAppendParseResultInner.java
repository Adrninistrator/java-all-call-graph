package com.adrninistrator.jacg.handler.dto.string;

import com.adrninistrator.jacg.handler.dto.string.element.BaseStringElement;

/**
 * @author adrninistrator
 * @date 2025/9/19
 * @description: 字符串拼接解析结果，方法内部处理使用
 */
public class StringAppendParseResultInner {

    // 原始字符串，对于枚举常量方法调用返回值，使用 {简单类名}.{枚举常量}.{被调用方法}() 形式
    private String rawString;

    // 解析后的字符串，对于枚举常量方法调用返回值，使用对应方法返回的常量值
    private String parsedValue;

    // 字符串元素
    private BaseStringElement stringElement;

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

    public BaseStringElement getStringElement() {
        return stringElement;
    }

    public void setStringElement(BaseStringElement stringElement) {
        this.stringElement = stringElement;
    }
}
