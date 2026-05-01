package com.adrninistrator.jacg.dto.callgraph;

/**
 * @author adrninistrator
 * @date 2026/4/19
 * @description: 生成调用链的方法与文件完整路径信息
 */
public class MethodCallGraphFilePathInfo {

    // 当前方法在输入任务中指定的内容（仅当当前方法在输入任务中指定的是方法形式时才有值）
    private final String origText;

    // 当前方法生成的完整调用链文件路径
    private final String filePath;

    public MethodCallGraphFilePathInfo(String origText, String filePath) {
        this.origText = origText;
        this.filePath = filePath;
    }

    public String getOrigText() {
        return origText;
    }

    public String getFilePath() {
        return filePath;
    }

    @Override
    public String toString() {
        return "MethodCallGraphFilePathInfo{" +
                "origText='" + origText + '\'' +
                ", filePath='" + filePath + '\'' +
                '}';
    }
}
