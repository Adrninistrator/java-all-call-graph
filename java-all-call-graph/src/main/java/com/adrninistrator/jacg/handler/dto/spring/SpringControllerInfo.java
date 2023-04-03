package com.adrninistrator.jacg.handler.dto.spring;

/**
 * @author adrninistrator
 * @date 2023/3/26
 * @description: Spring Controller信息
 */
public class SpringControllerInfo {
    private final String showUri;
    private final String fullMethod;

    public SpringControllerInfo(String showUri, String fullMethod) {
        this.showUri = showUri;
        this.fullMethod = fullMethod;
    }

    public String getShowUri() {
        return showUri;
    }

    public String getFullMethod() {
        return fullMethod;
    }
}
