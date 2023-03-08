package com.adrninistrator.jacg.reporter.common;

/**
 * @author adrninistrator
 * @date 2023/3/2
 * @description: 生成Spring调用报告的常量
 */
public class JACGSpReportConstants {
    public static final String DIR_NAME_SP_TX_USE_ANNOTATION = "使用注解@Transactional";
    public static final String DIR_NAME_SP_TX_USE_TPL = "使用事务模板TransactionTemplate";

    public static final String COLUMN_TX_TYPE = "事务使用方式";

    public static final String COLUMN_ANNOTATION_ENTRY_METHOD = "事务注解所在的入口方法";
    public static final String COLUMN_ANNOTATION_METHOD_PROPAGATION = "事务注解所在方法的事务传播行为";

    public static final String COLUMN_TPL_TYPE = "事务模板使用方式";
    public static final String COLUMN_TPL_CALLEE_FULL_METHOD = "事务模板中调用的完整方法";
    public static final String COLUMN_TPL_CALLER_FULL_METHOD = "调用事务模板的完整方法";
    public static final String COLUMN_TPL_CALLER_LINE_NUMBER = "调用事务模板的代码行号";

    private JACGSpReportConstants() {
        throw new IllegalStateException("illegal");
    }
}
