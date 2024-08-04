package com.adrninistrator.jacg.dto.callgraph;

import com.adrninistrator.jacg.dto.method.MethodDetail;

/**
 * @author adrninistrator
 * @date 2024/7/19
 * @description: 生成的调用链的JSON格式表示，方法信息
 */
public class CallGraphJsonMethod {

    private MethodDetail methodDetail;

    public CallGraphJsonMethod() {
    }

    public CallGraphJsonMethod(MethodDetail methodDetail) {
        this.methodDetail = methodDetail;
    }

    public MethodDetail getMethodDetail() {
        return methodDetail;
    }

    public void setMethodDetail(MethodDetail methodDetail) {
        this.methodDetail = methodDetail;
    }
}
