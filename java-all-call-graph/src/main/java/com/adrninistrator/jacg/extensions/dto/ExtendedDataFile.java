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

    // 自定义数据列表
    private List<ExtendedDataTypeAndValue> extendedDataTypeAndValueList;

    public String getFilePath() {
        return filePath;
    }

    public void setFilePath(String filePath) {
        this.filePath = filePath;
    }

    public List<ExtendedDataTypeAndValue> getExtendedDataTypeAndValueList() {
        return extendedDataTypeAndValueList;
    }

    public void setExtendedDataTypeAndValueList(List<ExtendedDataTypeAndValue> extendedDataTypeAndValueList) {
        this.extendedDataTypeAndValueList = extendedDataTypeAndValueList;
    }
}
