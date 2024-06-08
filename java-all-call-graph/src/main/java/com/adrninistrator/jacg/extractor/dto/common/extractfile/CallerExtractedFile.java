package com.adrninistrator.jacg.extractor.dto.common.extractfile;

import com.adrninistrator.jacg.extractor.dto.common.extract.CallerExtractedLine;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2021/10/19
 * @description: 向下的调用堆栈文件处理后的文件信息
 */
public class CallerExtractedFile extends AbstractCallGraphExtractedFile {
    // 向下的调用堆栈文件处理后对应行的信息列表
    private final List<CallerExtractedLine> callerExtractedLineList;

    public CallerExtractedFile(List<CallerExtractedLine> callerExtractedLineList) {
        this.callerExtractedLineList = callerExtractedLineList;
    }

    public List<CallerExtractedLine> getCallerExtractedLineList() {
        return callerExtractedLineList;
    }

    @Override
    public String toString() {
        return fullMethod + " [" + callerExtractedLineList.size() + "]";
    }
}
