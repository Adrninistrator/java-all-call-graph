package com.adrninistrator.jacg.dto.callstack;

import java.util.Collections;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2024/3/14
 * @description: 生成调用堆栈文件结果
 */
public class CallStackFileResult {

    // 代表处理失败的静态字段
    public static CallStackFileResult FAIL = new CallStackFileResult(false, null, null);

    // 代表处理成功但结果为空的静态字段
    public static CallStackFileResult EMPTY = new CallStackFileResult(true, Collections.emptyList(), Collections.emptyList());

    // 处理是否成功
    private final boolean success;

    // 生成的调用堆栈文件路径列表
    private final List<String> stackFilePathList;

    // 生成的单独的调用堆栈文件目录路径列表
    private final List<String> separateStackDirPathList;

    public CallStackFileResult(List<String> stackFilePathList, List<String> separateStackDirPathList) {
        this(true, stackFilePathList, separateStackDirPathList);
    }

    private CallStackFileResult(boolean success, List<String> stackFilePathList, List<String> separateStackDirPathList) {
        this.success = success;
        this.stackFilePathList = stackFilePathList;
        this.separateStackDirPathList = separateStackDirPathList;
    }

    public boolean isSuccess() {
        return success;
    }

    public List<String> getStackFilePathList() {
        return stackFilePathList;
    }

    public List<String> getSeparateStackDirPathList() {
        return separateStackDirPathList;
    }
}
