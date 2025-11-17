package com.adrninistrator.jacg.dto.callstack;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2024/3/14
 * @description: 生成调用堆栈文件结果
 */
public class CallStackFileResult {

    // 代表处理失败的静态字段
    public static CallStackFileResult FAIL = new CallStackFileResult(false, null, null, null);

    // 代表处理成功但结果为空的静态字段
    public static CallStackFileResult EMPTY = new CallStackFileResult(true, null, null, null);

    // 处理是否成功
    private final boolean success;

    // 记录当前生成方法完整调用链的目录
    private final String callGraphOutputDirPath;

    // 生成的调用堆栈文件路径列表
    private final List<String> stackFilePathList;

    // 生成的其他形式的调用堆栈文件目录路径列表
    private final List<String> otherFormsStackDirPathList;

    public CallStackFileResult(String callGraphOutputDirPath, List<String> stackFilePathList, List<String> otherFormsStackDirPathList) {
        this(true, callGraphOutputDirPath, stackFilePathList, otherFormsStackDirPathList);
    }

    private CallStackFileResult(boolean success, String callGraphOutputDirPath, List<String> stackFilePathList, List<String> otherFormsStackDirPathList) {
        this.success = success;
        this.callGraphOutputDirPath = callGraphOutputDirPath;
        this.stackFilePathList = stackFilePathList;
        this.otherFormsStackDirPathList = otherFormsStackDirPathList;
    }

    public boolean isSuccess() {
        return success;
    }

    public String getCallGraphOutputDirPath() {
        return callGraphOutputDirPath;
    }

    public List<String> getStackFilePathList() {
        return stackFilePathList;
    }

    public List<String> getOtherFormsStackDirPathList() {
        return otherFormsStackDirPathList;
    }
}
