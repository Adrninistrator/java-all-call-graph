package com.adrninistrator.jacg.reporter.common;

/**
 * @author adrninistrator
 * @date 2023/3/2
 * @description: 生成调用报告的常量
 */
public class JACGReportConstants {
    public static final String COLUMN_FULL_METHOD = "完整方法";
    public static final String COLUMN_CALLER_FULL_METHOD = "调用方完整方法";
    public static final String COLUMN_CALLER_LINE_NUMBER = "调用方代码行号";
    public static final String COLUMN_CALLEE_FULL_METHOD = "被调用完整方法";
    public static final String COLUMN_CALLEE_UPPER_FULL_METHOD = "被调用上一层方法";
    public static final String COLUMN_DATA_SEQ = "在调用堆栈文件中的数据序号";
    public static final String COLUMN_LINE_NUMBER = "在调用堆栈文件中的行号";
    public static final String COLUMN_FILE_PATH = "调用堆栈文件路径";
    public static final String COLUMN_RUN_IN_OTHER_THREAD = "在其他线程执行(run_in_other_thread)";
    public static final String COLUMN_METHOD_FLAGS = "方法标志";

    private JACGReportConstants() {
        throw new IllegalStateException("illegal");
    }
}
