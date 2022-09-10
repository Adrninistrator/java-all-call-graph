package com.adrninistrator.jacg.extractor.dto.result;

/**
 * @author adrninistrator
 * @date 2022/8/29
 * @description: 调用链结果文件信息基类
 */
public abstract class BaseResultFile {

    // 当前处理的查找关键字生成文件的路径
    private String filePath;

    // 当前处理的查找关键字生成文件的名称
    private String fileName;

    // 当前处理的查找关键字生成文件是否为空文件
    private boolean emptyFile;

    // 当前处理的文件对应的类名（完整类名或简单类名 ）
    private String className;

    // 当前处理的文件对应的完整类名
    private String fullClassName;

    // 当前处理的文件对应的方法名
    private String methodName;

    // 当前处理的文件对应的完整方法HASH+长度
    private String methodHash;

    // 当前处理的文件对应的完整方法
    private String fullMethod;

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

    public String getClassName() {
        return className;
    }

    public void setClassName(String className) {
        this.className = className;
    }

    public String getFullClassName() {
        return fullClassName;
    }

    public void setFullClassName(String fullClassName) {
        this.fullClassName = fullClassName;
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
}
