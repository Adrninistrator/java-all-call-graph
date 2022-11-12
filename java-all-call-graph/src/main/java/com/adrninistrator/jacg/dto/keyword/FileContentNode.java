package com.adrninistrator.jacg.dto.keyword;

/**
 * @author adrninistrator
 * @date 2022/8/26
 * @description: 搜索方法完整调用链文件时使用的节点
 */
public class FileContentNode {

    // 当前节点的父节点
    private final FileContentNode parentNode;

    // 方法级别
    private final int methodLevel;

    // 文件行内容
    private final String fileLineContent;

    public FileContentNode(FileContentNode parentNode, int methodLevel, String fileLineContent) {
        this.parentNode = parentNode;
        this.methodLevel = methodLevel;
        this.fileLineContent = fileLineContent;
    }

    //
    public FileContentNode getParentNode() {
        return parentNode;
    }

    public int getMethodLevel() {
        return methodLevel;
    }

    public String getFileLineContent() {
        return fileLineContent;
    }
}
