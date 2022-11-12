package com.adrninistrator.jacg.extractor.dto.result;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2021/10/19
 * @description:
 */
public class CallerExtendedDataFile extends BaseResultFile {
    // 自定义数据列表
    private List<CallerExtendedDataInfo> callerExtendedDataInfoList;

    public List<CallerExtendedDataInfo> getCallerExtendedDataInfoList() {
        return callerExtendedDataInfoList;
    }

    public void setCallerExtendedDataInfoList(List<CallerExtendedDataInfo> callerExtendedDataInfoList) {
        this.callerExtendedDataInfoList = callerExtendedDataInfoList;
    }
}
