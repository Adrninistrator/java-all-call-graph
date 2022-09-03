package com.adrninistrator.jacg.dto.keyword;

/**
 * @author adrninistrator
 * @date 2022/8/26
 * @description: 搜索方法完整调用链文件时使用的节点
 */
public class FileContentNode {

    // 当前节点的父节点
    private FileContentNode parentNode;

    // 方法级别
    private int methodLevel;

    // 文件行内容
    private String fileLineContent;

    public static FileContentNode genInstance(FileContentNode parentNode, int methodLevel, String fileLineContent) {
        FileContentNode fileContentNode = new FileContentNode();
        fileContentNode.setParentNode(parentNode);
        fileContentNode.setMethodLevel(methodLevel);
        fileContentNode.setFileLineContent(fileLineContent);

        return fileContentNode;
    }

    //
    public FileContentNode getParentNode() {
        return parentNode;
    }

    public void setParentNode(FileContentNode parentNode) {
        this.parentNode = parentNode;
    }

    public int getMethodLevel() {
        return methodLevel;
    }

    public void setMethodLevel(int methodLevel) {
        this.methodLevel = methodLevel;
    }

    public String getFileLineContent() {
        return fileLineContent;
    }

    public void setFileLineContent(String fileLineContent) {
        this.fileLineContent = fileLineContent;
    }
}
