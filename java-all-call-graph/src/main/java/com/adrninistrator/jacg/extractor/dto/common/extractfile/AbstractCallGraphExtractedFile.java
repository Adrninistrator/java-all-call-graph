package com.adrninistrator.jacg.extractor.dto.common.extractfile;

/**
 * @author adrninistrator
 * @date 2022/8/29
 * @description: 调用堆栈文件处理结果信息基类
 */
public abstract class AbstractCallGraphExtractedFile {
    // 当前处理的调用堆栈文件的路径
    protected String stackFilePath;

    // 当前处理的调用堆栈文件的名称
    protected String stackFileName;

    // 当前处理的调用堆栈文件是否为空文件
    protected boolean emptyStackFile;

    // 当前处理的调用堆栈文件对应的类名（完整类名或简单类名 ）
    protected String simpleClassName;

    // 当前处理的调用堆栈文件对应的完整类名
    protected String className;

    // 当前处理的调用堆栈文件对应的方法名
    protected String methodName;

    // 当前处理的调用堆栈文件对应的完整方法HASH+长度
    protected String methodHash;

    // 当前处理的调用堆栈文件对应的完整方法
    protected String fullMethod;

    // 当前处理的调用堆栈文件对应的方法返回类型
    protected String returnType;

    /**
     * 拷贝基础数据
     *
     * @param src 原始对象
     */
    public void copy(AbstractCallGraphExtractedFile src) {
        this.setStackFilePath(src.getStackFilePath());
        this.setStackFileName(src.getStackFileName());
        this.setSimpleClassName(src.getSimpleClassName());
        this.setClassName(src.getClassName());
        this.setMethodName(src.getMethodName());
        this.setMethodHash(src.getMethodHash());
        this.setFullMethod(src.getFullMethod());
        this.setReturnType(src.getReturnType());
    }

    public String getStackFilePath() {
        return stackFilePath;
    }

    public void setStackFilePath(String stackFilePath) {
        this.stackFilePath = stackFilePath;
    }

    public String getStackFileName() {
        return stackFileName;
    }

    public void setStackFileName(String stackFileName) {
        this.stackFileName = stackFileName;
    }

    public boolean isEmptyStackFile() {
        return emptyStackFile;
    }

    public void setEmptyStackFile(boolean emptyStackFile) {
        this.emptyStackFile = emptyStackFile;
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

    public String getReturnType() {
        return returnType;
    }

    public void setReturnType(String returnType) {
        this.returnType = returnType;
    }

    @Override
    public String toString() {
        return fullMethod;
    }
}
