package com.adrninistrator.jacg.common;

/**
 * @author adrninistrator
 * @date 2021/6/17
 * @description:
 */

public class JACGConstants {
    public static final String DIR_OUTPUT_GRAPH_FOR_CALLEE = "_jacg_o_ee";
    public static final String DIR_OUTPUT_GRAPH_FOR_CALLER = "_jacg_o_er";
    public static final String DIR_OUTPUT_METHODS = "methods";
    public static final String DIR_OUTPUT_STACK = "_stack";
    public static final String DIR_KEYWORDS_NOT_FOUND = "_keywords_not_found";

    // 保存全部的配置参数信息文件
    public static final String FILE_JACG_ALL_CONFIG_MD = "_jacg_all_config.md";
    // 保存当前有使用的配置参数信息文件
    public static final String FILE_JACG_USED_CONFIG_MD = "_jacg_used_config.md";
    public static final String FILE_JAVACG_USED_CONFIG_MD = "_javacg_used_config.md";

    public static final String USED_CONFIG_FLAG_FILE_KEY = "配置文件名称";
    public static final String USED_CONFIG_FLAG_FILE_DESC = "配置文件说明";
    public static final String USED_CONFIG_FLAG_CONF_KEY = "参数名称";
    public static final String USED_CONFIG_FLAG_CONF_DESC = "参数说明";
    public static final String USED_CONFIG_FLAG_CONF_VALUE = "参数值";
    public static final String USED_CONFIG_FLAG_CONF_LIST = "区分顺序的其他配置信息";
    public static final String USED_CONFIG_FLAG_CONF_SET = "不区分顺序的其他配置信息";

    // sql语句中用于替换的appName
    public static final String APP_NAME_IN_SQL = "{appName}";

    public static final String SQL_CREATE_TABLE_HEAD = "CREATE TABLE if not exists";
    public static final String SQL_ENGINE_INNODB = "ENGINE=InnoDB";

    public static final String FLAG_LEFT_PARENTHESES = "[";
    public static final String FLAG_RIGHT_PARENTHESES = "]";
    public static final String FLAG_LEFT_BIG_PARENTHESES = "{";
    public static final String FLAG_RIGHT_BIG_PARENTHESES = "}";
    public static final String FLAG_SPACE = " ";
    public static final String FLAG_AT = "@";
    public static final String FLAG_AT_FULL_WIDTH = "＠";
    public static final String FLAG_MINUS = "-";
    public static final String FLAG_UNDER_LINE = "_";
    public static final String FLAG_TAB = "\t";
    public static final String FLAG_COMMA_WITH_SPACE = ", ";

    public static final char FLAG_CHAR_SPACE = FLAG_SPACE.charAt(0);

    public static final String FLAG_MD_LINE_NUMBER = "调用链文件行号: ";

    public static final String FLAG_EMPTY = "-empty";

    public static final String EXT_TXT = ".txt";
    public static final String EXT_EMPTY_TXT = FLAG_EMPTY + EXT_TXT;
    public static final String EXT_MD = ".md";
    public static final String EXT_EMPTY_MD = FLAG_EMPTY + EXT_MD;
    public static final String EXT_SQL = ".sql";

    public static final String NEW_LINE = "\n";

    // 调用链文件中，每个级别之间的缩进，两个空格
    public static final String OUTPUT_SPLIT_FLAG = FLAG_SPACE + FLAG_SPACE;

    public static final String CALLEE_FLAG_ENTRY_NO_TAB = "!entry!";
    public static final String CALLEE_FLAG_ENTRY = FLAG_TAB + CALLEE_FLAG_ENTRY_NO_TAB;
    public static final String CALL_FLAG_CYCLE_START = "!cycle" + JACGConstants.FLAG_LEFT_PARENTHESES;
    public static final String CALL_FLAG_CYCLE_END = JACGConstants.FLAG_RIGHT_PARENTHESES + "!";
    public static final String CALL_FLAG_CYCLE = CALL_FLAG_CYCLE_START + "%d" + CALL_FLAG_CYCLE_END;
    public static final String CALL_FLAG_BUSINESS_DATA = "!busi_data!";
    public static final String CALL_FLAG_RUN_IN_OTHER_THREAD_NO_TAB = "!run_in_other_thread!";
    public static final String CALL_FLAG_RUN_IN_OTHER_THREAD = FLAG_TAB + CALL_FLAG_RUN_IN_OTHER_THREAD_NO_TAB;
    public static final String CALL_FLAG_RUN_IN_TRANSACTION_NO_TAB = "!run_in_transaction!";
    public static final String CALL_FLAG_RUN_IN_TRANSACTION = FLAG_TAB + CALL_FLAG_RUN_IN_TRANSACTION_NO_TAB;

    public static final String MYSQL_FLAG = "mysql";
    public static final String MYSQL_REWRITEBATCHEDSTATEMENTS = "rewriteBatchedStatements=true";

    // 代表不存在循环的方法调用的值
    public static final int NO_CYCLE_CALL_FLAG = -1;

    // 允许使用的最大线程数
    public static final int MAX_THREAD_NUM = 100;
    // 生成调用链文件时，记录数达到多少以后打印日志
    public static final int NOTICE_LINE_NUM = 5000;
    // 批量写入数据库时每次插入的数量允许的最大值
    public static final int MAX_DB_INSERT_BATCH_SIZE = 5000;
    // 从数据库分页查询数据的数量
    public static final int DB_QUERY_PAGE_SIZE = 1000;

    // 代表分页查询失败
    public static final int PAGE_QUERY_FAIL = -2;
    // 代表最后一次分页查询
    public static final int PAGE_QUERY_LAST = -1;

    // 存在一对多的方法调用提示文件，接口调用实现类
    public static final String NOTICE_MULTI_ITF_MD = "_notice_multi_ITF.md";
    // 存在一对多的方法调用提示文件，父类调用子类
    public static final String NOTICE_MULTI_SCC_MD = "_notice_multi_SCC.md";
    // 一对多的方法调用被禁用提示文件，接口调用实现类
    public static final String NOTICE_DISABLED_ITF_MD = "_notice_disabled_ITF.md";
    // 一对多的方法调用被禁用提示文件，父类调用子类
    public static final String NOTICE_DISABLED_SCC_MD = "_notice_disabled_SCC.md";

    // 代表未获取到代码行号
    public static final int LINE_NUM_NONE = -1;
    // 调用链搜索文件文件中，代表未处理数据序号值
    public static final int DATA_SEQ_NONE = -1;

    // 向下的方法完整调用链文件名，使用"@"进行分隔后最小的列数
    public static final int CALLER_FILE_NAME_SPLIT_BY_AT_MIN_COLUMNS = 3;

    // 方法调用表最大序号，代表非法的值
    public static final int MAX_METHOD_CALL_ID_ILLEGAL = -1;

    // 方法完整调用链文件中的级别，代表起始的值
    public static final int CALL_GRAPH_METHOD_LEVEL_START = 0;

    // 方法完整调用链文件中的级别，代表非法的值
    public static final int CALL_GRAPH_METHOD_LEVEL_ILLEGAL = -1;

    // 向上的方法完整调用链文件，每行内容的最小列数
    public static final int CALL_GRAPH_EE_LINE_MIN_COLUMN_NUM = 1;

    // 向下的方法完整调用链文件，每行内容的最小列数
    public static final int CALL_GRAPH_ER_LINE_MIN_COLUMN_NUM = 1;

    // 输出的注解信息文件，包含属性时的列数
    public static final int ANNOTATION_COLUMN_NUM_WITH_ATTRIBUTE = 5;
    // 输出的注解信息文件，不包含属性时的列数
    public static final int ANNOTATION_COLUMN_NUM_WITHOUT_ATTRIBUTE = 2;

    public static final String H2_PROTOCOL = "jdbc:h2:file:";
    public static final String H2_SCHEMA = "jacg";
    public static final String H2_FILE_EXT = ".mv.db";

    public static final String THREAD_NAME_PREFIX_WORKER = "jacg_worker";

    private JACGConstants() {
        throw new IllegalStateException("illegal");
    }
}
