package com.adrninistrator.jacg.handler.dto.spring;

/**
 * @author adrninistrator
 * @date 2024/1/25
 * @description: Spring Controller详细信息
 */
public class SpringControllerInfoDetail extends SpringControllerInfo {
    private final String returnType;

    public SpringControllerInfoDetail(String showUri, String fullMethod, String returnType) {
        super(showUri, fullMethod);
        this.returnType = returnType;
    }

    public String getReturnType() {
        return returnType;
    }
}
