package com.adrninistrator.jacg.handler.dto.spring;

import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4SpringController;

/**
 * @author adrninistrator
 * @date 2024/1/25
 * @description: Spring Controller方法详情，包括对应的方法信息
 */
public class SpringControllerInfoDetail {
    private WriteDbData4SpringController springController;
    private WriteDbData4MethodInfo methodInfo;

    public WriteDbData4SpringController getSpringController() {
        return springController;
    }

    public void setSpringController(WriteDbData4SpringController springController) {
        this.springController = springController;
    }

    public WriteDbData4MethodInfo getMethodInfo() {
        return methodInfo;
    }

    public void setMethodInfo(WriteDbData4MethodInfo methodInfo) {
        this.methodInfo = methodInfo;
    }
}
