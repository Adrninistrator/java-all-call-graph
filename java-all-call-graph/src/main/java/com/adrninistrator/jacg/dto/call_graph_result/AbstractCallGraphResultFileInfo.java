package com.adrninistrator.jacg.dto.call_graph_result;

/**
 * @author adrninistrator
 * @date 2022/8/29
 * @description: 调用链结果文件信息基类
 */
public abstract class AbstractCallGraphResultFileInfo {
    // 当前处理的文件的路径
    protected String filePath;

    // 当前处理的文件的名称
    protected String fileName;

    // 当前处理的文件是否为空文件
    protected boolean emptyFile;

    // 当前处理的文件对应的类名（完整类名或简单类名 ）
    protected String simpleClassName;

    // 当前处理的文件对应的完整类名
    protected String className;

    // 当前处理的文件对应的方法名
    protected String methodName;

    // 当前处理的文件对应的完整方法HASH+长度
    protected String methodHash;

    // 当前处理的文件对应的完整方法
    protected String fullMethod;

    /**
     * 拷贝基础数据
     *
     * @param src  原始对象
     * @param dest 目标对象
     */
    public static void copy(AbstractCallGraphResultFileInfo src, AbstractCallGraphResultFileInfo dest) {
        dest.setFilePath(src.getFilePath());
        dest.setFileName(src.getFileName());
        dest.setSimpleClassName(src.getSimpleClassName());
        dest.setClassName(src.getClassName());
        dest.setMethodName(src.getMethodName());
        dest.setMethodHash(src.getMethodHash());
        dest.setFullMethod(src.getFullMethod());
    }

    public String getFilePath() {
        return filePath;
    }

    public void setFilePath(String filePath) {
        this.filePath = filePath;
    }

    public String getFileName() {
        return fileName;
    }

    public void setFileName(String fileName) {
        this.fileName = fileName;
    }

    public boolean isEmptyFile() {
        return emptyFile;
    }

    public void setEmptyFile(boolean emptyFile) {
        this.emptyFile = emptyFile;
    }

    public String getSimpleClassName() {
        return simpleClassName;
    }

    public void setSimpleClassName(String simpleClassName) {
        this.simpleClassName = simpleClassName;
    }

    public String getClassName() {
        return className;
    }

    public void setClassName(String className) {
        this.className = className;
    }

    public String getMethodName() {
        return methodName;
    }

    public void setMethodName(String methodName) {
        this.methodName = methodName;
    }

    public String getMethodHash() {
        return methodHash;
    }

    public void setMethodHash(String methodHash) {
        this.methodHash = methodHash;
    }

    public String getFullMethod() {
        return fullMethod;
    }

    public void setFullMethod(String fullMethod) {
        this.fullMethod = fullMethod;
    }

    @Override
    public String toString() {
        return fullMethod;
    }
}
