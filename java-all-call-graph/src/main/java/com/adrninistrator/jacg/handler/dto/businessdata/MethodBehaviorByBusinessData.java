package com.adrninistrator.jacg.handler.dto.businessdata;

/**
 * @author adrninistrator
 * @date 2023/9/10
 * @description: 通过业务功能数据获取到的方法操作
 */
public class MethodBehaviorByBusinessData {

    // 类型
    protected final String type;

    public MethodBehaviorByBusinessData(String type) {
        this.type = type;
    }

    public String getType() {
        return type;
    }
}
