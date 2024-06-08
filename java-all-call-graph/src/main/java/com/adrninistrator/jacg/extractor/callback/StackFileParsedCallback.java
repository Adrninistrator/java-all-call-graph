package com.adrninistrator.jacg.extractor.callback;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2024/4/27
 * @description: 处理解析后的调用堆栈文件的回调接口
 */
public interface StackFileParsedCallback {

    /**
     * 对当前的调用堆栈数据进行处理
     *
     * @param dataSeq          调用堆栈文件中的数据序号
     * @param lineList         调用堆栈文件中的调用堆栈数据列表
     * @param lineNumberList   调用堆栈文件中的调用堆栈数据对应的行号列表
     * @param runInOtherThread 是否在其他线程执行
     * @param runInTransaction 是否在事务中执行
     * @param args             自定义参数
     */
    void handleCallStackData(int dataSeq, List<String> lineList, List<Integer> lineNumberList, boolean runInOtherThread, boolean runInTransaction,
                             Object... args);
}
