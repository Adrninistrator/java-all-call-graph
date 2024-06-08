package com.adrninistrator.jacg.extractor.callback;

import com.adrninistrator.jacg.extractor.dto.common.extractfile.CallerExtractedFile;

/**
 * @author adrninistrator
 * @date 2023/9/10
 * @description: 对向下的方法调用链文件进行数据提取时，对处理后的文件进行自定义处理的回调类接口
 */
public interface CallerExtractedFileCallback {

    /**
     * 对向下的方法调用链处理后的文件进行自定义处理
     *
     * @param callerExtractedFile 向下的方法调用链处理后的文件
     * @return true: 处理成功 false: 处理失败，会结束处理流程
     */
    boolean handle(CallerExtractedFile callerExtractedFile);
}
