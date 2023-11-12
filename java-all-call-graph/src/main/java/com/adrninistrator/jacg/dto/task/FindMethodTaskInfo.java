package com.adrninistrator.jacg.dto.task;

import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2022/4/29
 * @description:
 */
public class FindMethodTaskInfo {
    // 是否出现错误
    private boolean error;

    // 是否需要生成空文件（只要不是无法执行下去的错误，都生成空文件，避免终止处理）
    private boolean genEmptyFile;

    // 用于查找方法的任务元素
    private final List<FindMethodTaskElement> taskElementList = new ArrayList<>();

    public static FindMethodTaskInfo genFindMethodInfoFail() {
        return genFindMethodInfo(true, false);
    }

    public static FindMethodTaskInfo genFindMethodInfoGenEmptyFile() {
        return genFindMethodInfo(false, true);
    }

    public static FindMethodTaskInfo genFindMethodInfoSuccess() {
        return genFindMethodInfo(false, false);
    }

    private static FindMethodTaskInfo genFindMethodInfo(boolean error, boolean genEmptyFile) {
        FindMethodTaskInfo findMethodTaskInfo = new FindMethodTaskInfo();
        findMethodTaskInfo.setError(error);
        findMethodTaskInfo.setGenEmptyFile(genEmptyFile);
        return findMethodTaskInfo;
    }

    public void addTaskElement(String methodHash, String fullMethod, int callFlags, String returnType) {
        taskElementList.add(new FindMethodTaskElement(methodHash, fullMethod, callFlags, returnType));
    }

    //
    public boolean isError() {
        return error;
    }

    public void setError(boolean error) {
        this.error = error;
    }

    public boolean isGenEmptyFile() {
        return genEmptyFile;
    }

    public void setGenEmptyFile(boolean genEmptyFile) {
        this.genEmptyFile = genEmptyFile;
    }

    public List<FindMethodTaskElement> getTaskElementList() {
        return taskElementList;
    }
}
