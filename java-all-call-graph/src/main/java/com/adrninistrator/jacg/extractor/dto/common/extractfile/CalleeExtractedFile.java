package com.adrninistrator.jacg.extractor.dto.common.extractfile;

import com.adrninistrator.jacg.extractor.dto.common.extract.CalleeExtractedLine;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2022/8/29
 * @description: 向上的调用堆栈文件处理后的文件信息
 */
public class CalleeExtractedFile extends AbstractCallGraphExtractedFile {
    // 向下的调用堆栈文件处理后对应行的信息列表
    private final List<CalleeExtractedLine> calleeExtractedLineList;

    public CalleeExtractedFile(List<CalleeExtractedLine> calleeExtractedLineList) {
        this.calleeExtractedLineList = calleeExtractedLineList;
    }

    public List<CalleeExtractedLine> getCalleeExtractedLineList() {
        return calleeExtractedLineList;
    }

    @Override
    public String toString() {
        return fullMethod + " [" + calleeExtractedLineList.size() + "]";
    }
}
