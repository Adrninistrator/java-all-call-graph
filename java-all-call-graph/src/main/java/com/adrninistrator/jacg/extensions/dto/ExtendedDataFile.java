package com.adrninistrator.jacg.extensions.dto;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2021/10/19
 * @description:
 */
public class ExtendedDataFile {

    // 当前处理的查找关键字生成文件的路径
    private String filePath;

    // 当前处理的查找关键字生成文件的名称
    private String fileName;

    // 自定义数据列表
    private List<ExtendedDataInfo> extendedDataInfoList;

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

    public List<ExtendedDataInfo> getExtendedDataInfoList() {
        return extendedDataInfoList;
    }

    public void setExtendedDataInfoList(List<ExtendedDataInfo> extendedDataInfoList) {
        this.extendedDataInfoList = extendedDataInfoList;
    }
}
