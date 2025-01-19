package com.adrninistrator.jacg.extractor.dto.common.extract;

import com.adrninistrator.jacg.dto.callline.CallGraphLineParsed;

/**
 * @author adrninistrator
 * @date 2022/8/29
 * @description: 向上的调用堆栈文件处理后对应行的信息
 */
public class CalleeExtractedLine extends BaseCallGraphExtractedLine {
    // 入口方法是否存在父类/接口调用子类/实现类的调用关系
    private boolean entryMethodExistsSuperCallChild = false;

    // 入口方法是否定义在父类或接口中（不在当前对应的类中）
    private boolean entryMethodDefineInSuperInterface =false;

    // 任务指定的被调用方法的直接调用方法对应行的完整内容
    private String directlyCallerLineContent;

    // 任务指定的被调用方法的直接调用方法对应行解析后的内容
    private CallGraphLineParsed directlyCallerLineParsed;

    public boolean isEntryMethodExistsSuperCallChild() {
        return entryMethodExistsSuperCallChild;
    }

    public void setEntryMethodExistsSuperCallChild(boolean entryMethodExistsSuperCallChild) {
        this.entryMethodExistsSuperCallChild = entryMethodExistsSuperCallChild;
    }

    public boolean isEntryMethodDefineInSuperInterface() {
        return entryMethodDefineInSuperInterface;
    }

    public void setEntryMethodDefineInSuperInterface(boolean entryMethodDefineInSuperInterface) {
        this.entryMethodDefineInSuperInterface = entryMethodDefineInSuperInterface;
    }

    public String getDirectlyCallerLineContent() {
        return directlyCallerLineContent;
    }

    public void setDirectlyCallerLineContent(String directlyCallerLineContent) {
        this.directlyCallerLineContent = directlyCallerLineContent;
    }

    public CallGraphLineParsed getDirectlyCallerLineParsed() {
        return directlyCallerLineParsed;
    }

    public void setDirectlyCallerLineParsed(CallGraphLineParsed directlyCallerLineParsed) {
        this.directlyCallerLineParsed = directlyCallerLineParsed;
    }

    @Override
    public String toString() {
        return "CalleeExtractedLine{" +
                "dataSeq=" + dataSeq +
                ", lineNumber=" + lineNumber +
                ", lineContent='" + lineContent + '\'' +
                '}';
    }
}
