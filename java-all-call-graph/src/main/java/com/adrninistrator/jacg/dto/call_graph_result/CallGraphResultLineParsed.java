package com.adrninistrator.jacg.dto.call_graph_result;

import com.adrninistrator.jacg.dto.method.MethodDetail;
import com.adrninistrator.jacg.extensions.dto.extened_data.BaseExtendedData;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2022/12/10
 * @description: 方法完整调用链文件中每行解析后的内容
 */
public class CallGraphResultLineParsed {
    // 方法级别
    private int methodLevel;

    // 调用或被调用方法详情
    private MethodDetail methodDetail;

    // 方法上的注解信息（可能包含多个）
    private String[] annotations;

    // 方法调用自定义数据列表
    private BaseExtendedData extendedData;

    // 是否出现循环调用
    private boolean cycleCall;

    // 循环调用对应的方法级别
    private int cycleCallLevel;

    // 是否入口方法（向上的完整方法调用链有效）
    private boolean entryMethod;

    public int getMethodLevel() {
        return methodLevel;
    }

    public void setMethodLevel(int methodLevel) {
        this.methodLevel = methodLevel;
    }

    public MethodDetail getMethodDetail() {
        return methodDetail;
    }

    public void setMethodDetail(MethodDetail methodDetail) {
        this.methodDetail = methodDetail;
    }

    public String[] getAnnotations() {
        return annotations;
    }

    public void setAnnotations(String[] annotations) {
        this.annotations = annotations;
    }

    public BaseExtendedData getExtendedData() {
        return extendedData;
    }

    public void setExtendedData(BaseExtendedData extendedData) {
        this.extendedData = extendedData;
    }

    public boolean isCycleCall() {
        return cycleCall;
    }

    public void setCycleCall(boolean cycleCall) {
        this.cycleCall = cycleCall;
    }

    public int getCycleCallLevel() {
        return cycleCallLevel;
    }

    public void setCycleCallLevel(int cycleCallLevel) {
        this.cycleCallLevel = cycleCallLevel;
    }

    public boolean isEntryMethod() {
        return entryMethod;
    }

    public void setEntryMethod(boolean entryMethod) {
        this.entryMethod = entryMethod;
    }
}
