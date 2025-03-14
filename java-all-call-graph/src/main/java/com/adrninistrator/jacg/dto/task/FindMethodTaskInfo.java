package com.adrninistrator.jacg.dto.task;

import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2022/4/29
 * @description: 用于查找方法的任务信息
 */
public class FindMethodTaskInfo {

    // 是否需要生成空文件（只要不是无法执行下去的错误，都生成空文件，避免终止处理）
    private boolean genNotFoundFile;

    // 用于查找方法的任务元素
    private final List<FindMethodTaskElement> taskElementList = new ArrayList<>();

    public static FindMethodTaskInfo genFindMethodInfoGenNotFoundFile() {
        return genFindMethodInfo(true);
    }

    public static FindMethodTaskInfo genFindMethodInfoSuccess() {
        return genFindMethodInfo(false);
    }

    private static FindMethodTaskInfo genFindMethodInfo(boolean genNotFoundFile) {
        FindMethodTaskInfo findMethodTaskInfo = new FindMethodTaskInfo();
        findMethodTaskInfo.setGenNotFoundFile(genNotFoundFile);
        return findMethodTaskInfo;
    }

    public void addTaskElement(String methodHash, String fullMethod, String returnType) {
        taskElementList.add(new FindMethodTaskElement(methodHash, fullMethod, returnType));
    }

    //
    public boolean isGenNotFoundFile() {
        return genNotFoundFile;
    }

    public void setGenNotFoundFile(boolean genNotFoundFile) {
        this.genNotFoundFile = genNotFoundFile;
    }

    public List<FindMethodTaskElement> getTaskElementList() {
        return taskElementList;
    }
}
