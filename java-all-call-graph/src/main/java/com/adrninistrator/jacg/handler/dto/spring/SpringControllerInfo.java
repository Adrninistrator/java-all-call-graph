package com.adrninistrator.jacg.handler.dto.spring;

/**
 * @author adrninistrator
 * @date 2023/3/26
 * @description: Spring Controller信息
 */
public class SpringControllerInfo {
    private final String showUri;
    private final String fullMethod;
    private final String returnType;

    public SpringControllerInfo(String showUri, String fullMethod, String returnType) {
        this.showUri = showUri;
        this.fullMethod = fullMethod;
        this.returnType = returnType;
    }

    public String getShowUri() {
        return showUri;
    }

    public String getFullMethod() {
        return fullMethod;
    }

    public String getReturnType() {
        return returnType;
    }
}
