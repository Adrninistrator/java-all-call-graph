package com.adrninistrator.jacg.dto.callline;

import com.adrninistrator.jacg.dto.method.MethodDetail;
import com.adrninistrator.jacg.handler.dto.businessdata.BaseBusinessData;
import com.adrninistrator.javacg2.common.JavaCG2Constants;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2022/12/10
 * @description: 方法完整调用链文件中每行解析后的内容
 */
public class CallGraphLineParsed {
    // 方法级别
    private int methodLevel;

    // 调用或被调用方法详情
    private MethodDetail methodDetail;

    // 调用方法代码行号
    private Integer callerLineNumber;

    // 方法上的注解信息（可能包含多个）
    private String[] annotations;

    // 方法调用业务功能数据列表
    private List<BaseBusinessData> businessDataList;

    // 循环调用对应的方法级别，非空时代表出现循环调用
    private Integer cycleCallLevel;

    // 是否入口方法（向上的方法完整调用链有效）
    private boolean entryMethodFlag;

    // 是否在其他线程执行
    private boolean runInOtherThreadFlag;

    // 是否在事务中执行
    private boolean runInTransactionFlag;

    public String getCallerLineNumberStr() {
        if (callerLineNumber == null) {
            return JavaCG2Constants.DEFAULT_LINE_NUMBER_STR;
        }
        return String.valueOf(callerLineNumber);
    }

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

    public Integer getCallerLineNumber() {
        return callerLineNumber;
    }

    public void setCallerLineNumber(Integer callerLineNumber) {
        this.callerLineNumber = callerLineNumber;
    }

    public String[] getAnnotations() {
        return annotations;
    }

    public void setAnnotations(String[] annotations) {
        this.annotations = annotations;
    }

    public List<BaseBusinessData> getBusinessDataList() {
        return businessDataList;
    }

    public void setBusinessDataList(List<BaseBusinessData> businessDataList) {
        this.businessDataList = businessDataList;
    }

    public Integer getCycleCallLevel() {
        return cycleCallLevel;
    }

    public void setCycleCallLevel(Integer cycleCallLevel) {
        this.cycleCallLevel = cycleCallLevel;
    }

    public boolean isEntryMethodFlag() {
        return entryMethodFlag;
    }

    public void setEntryMethodFlag(boolean entryMethodFlag) {
        this.entryMethodFlag = entryMethodFlag;
    }

    public boolean isRunInOtherThreadFlag() {
        return runInOtherThreadFlag;
    }

    public void setRunInOtherThreadFlag(boolean runInOtherThreadFlag) {
        this.runInOtherThreadFlag = runInOtherThreadFlag;
    }

    public boolean isRunInTransactionFlag() {
        return runInTransactionFlag;
    }

    public void setRunInTransactionFlag(boolean runInTransactionFlag) {
        this.runInTransactionFlag = runInTransactionFlag;
    }
}
