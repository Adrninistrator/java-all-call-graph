package com.adrninistrator.jacg.common;

import com.adrninistrator.javacg2.common.JavaCG2Constants;

/**
 * @author adrninistrator
 * @date 2021/6/17
 * @description:
 */

public class JACGConstants {
    public static final String DIR_OUTPUT_GRAPH_FOR_CALLEE = "_jacg_o_ee";
    public static final String DIR_OUTPUT_GRAPH_FOR_CALLER = "_jacg_o_er";
    public static final String DIR_OUTPUT_STACK = "_stack";
    public static final String DIR_OUTPUT_STACK_SEPARATE = "_separate";
    public static final String DIR_KEYWORDS_NOT_FOUND = "_keywords_not_found";
    public static final String DIR_OUTPUT_JSON = "_json";
    public static final String DIR_OUTPUT_UNITTEST = "_unittest_output";

    // 保存全部的配置参数信息文件
    public static final String FILE_JACG_ALL_CONFIG_MD = "_jacg_all_config.md";
    // 保存当前有使用的配置参数信息文件
    public static final String FILE_JACG_USED_CONFIG_MD = "_jacg_used_config.md";
    public static final String FILE_STACK_CALLEE_MD = "_stack_callee.md";
    public static final String FILE_STACK_CALLER_MD = "_stack_caller.md";
    public static final String FILE_STACK_SUMMARY_CALLEE_MD = "_summary_callee.md";
    public static final String FILE_STACK_SUMMARY_CALLER_MD = "_summary_caller.md";

    public static final String FILE_JAR_DIFF_MODIFIED_METHODS_BASE = "_modified_methods_base.md";
    public static final String FILE_JAR_DIFF_MODIFIED_METHODS_STACK = "_modified_methods_stack.md";

    public static final String JAR_DIFF_METHOD_EXISTED = "existed";
    public static final String JAR_DIFF_METHOD_NEW = "new";

    public static final String TABLE_PREFIX = "jacg_";

    // sql语句中用于替换的appName
    public static final String REPLACE_SQL_FLAG_APP_NAME = "{appName}";

    public static final String SQL_CREATE_TABLE_HEAD = "CREATE TABLE if not exists";

    public static final String FLAG_LEFT_PARENTHESES = "[";
    public static final String FLAG_RIGHT_PARENTHESES = "]";
    public static final String FLAG_LEFT_BIG_PARENTHESES = "{";
    public static final String FLAG_RIGHT_BIG_PARENTHESES = "}";
    public static final String FLAG_SPACE = " ";
    public static final String FLAG_AT = "@";
    public static final String FLAG_AT_FULL_WIDTH = "＠";
    public static final String FLAG_MINUS = "-";
    public static final String FLAG_UNDER_LINE = "_";
    public static final String FLAG_COMMA_WITH_SPACE = ", ";
    public static final String FLAG_FILE_PROTOCOL = "file:/";
    public static final String FLAG_EXCLAMATION = "!";

    public static final char FLAG_CHAR_SPACE = FLAG_SPACE.charAt(0);

    public static final String FLAG_MD_LINE_NUMBER = "调用链文件行号: ";

    public static final String FLAG_EMPTY = "!empty";
    public static final String FLAG_NOT_FOUND = "!not_found";

    public static final String EMPTY_TXT = FLAG_EMPTY + JavaCG2Constants.EXT_TXT;
    public static final String EMPTY_MD = FLAG_EMPTY + JavaCG2Constants.EXT_MD;
    public static final String NOT_FOUND_TXT = FLAG_NOT_FOUND + JavaCG2Constants.EXT_TXT;
    public static final String NOT_FOUND_MD = FLAG_NOT_FOUND + JavaCG2Constants.EXT_MD;

    public static final String EXT_SQL = ".sql";
    public static final String EXT_CLASS = ".class";
    public static final String EXT_XML = ".xml";
    public static final String EXT_PROPERTIES = ".properties";
    public static final String EXT_JSON = ".json";

    // 调用链文件中，每个级别之间的缩进，两个空格
    public static final String OUTPUT_SPLIT_FLAG = FLAG_SPACE + FLAG_SPACE;

    public static final String CALLEE_FLAG_ENTRY_NO_TAB = "!entry!";
    public static final String CALLEE_FLAG_ENTRY = JavaCG2Constants.FLAG_TAB + CALLEE_FLAG_ENTRY_NO_TAB;
    public static final String CALLEE_FLAG_NO_CALLEE_NO_TAB = "!no_callee!";
    public static final String CALLEE_FLAG_NO_CALLEE = JavaCG2Constants.FLAG_TAB + CALLEE_FLAG_NO_CALLEE_NO_TAB;
    public static final String CALL_FLAG_CYCLE_START = "!cycle" + JACGConstants.FLAG_LEFT_PARENTHESES;
    public static final String CALL_FLAG_CYCLE_END = JACGConstants.FLAG_RIGHT_PARENTHESES + "!";
    public static final String CALL_FLAG_CYCLE = CALL_FLAG_CYCLE_START + "%d" + CALL_FLAG_CYCLE_END;
    public static final String CALL_FLAG_BUSINESS_DATA = "!busi_data!";
    public static final String CALL_FLAG_RUN_IN_OTHER_THREAD_NO_TAB = "!run_in_other_thread!";
    public static final String CALL_FLAG_RUN_IN_OTHER_THREAD = JavaCG2Constants.FLAG_TAB + CALL_FLAG_RUN_IN_OTHER_THREAD_NO_TAB;
    public static final String CALL_FLAG_RUN_IN_SPRING_TX_NO_TAB = "!run_in_spring_tx!";
    public static final String CALL_FLAG_RUN_IN_SPRING_TX = JavaCG2Constants.FLAG_TAB + CALL_FLAG_RUN_IN_SPRING_TX_NO_TAB;

    public static final String MYSQL_FLAG = "mysql";
    public static final String MYSQL_REWRITEBATCHEDSTATEMENTS = "rewriteBatchedStatements=true";

    // 允许使用的最大线程数
    public static final int MAX_THREAD_NUM = 1000;
    // 生成调用链文件时，记录数达到多少以后打印日志
    public static final int NOTICE_LINE_NUM = 5000;
    // 批量写入数据库时每次插入的数量允许的最大值
    public static final int MAX_DB_INSERT_BATCH_SIZE = 50000;
    // 数据库分页操作数量
    public static final int DB_PAGE_HANDLE_SIZE = 1000;
    public static final int DB_PAGE_HANDLE_SIZE_MINUS_1 = DB_PAGE_HANDLE_SIZE - 1;
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

    // 向下的方法完整调用链文件名，使用"@"进行分隔后的列数，1列
    public static final int CALL_GRAPH_FILE_NAME_COLUMNS_1 = 1;
    // 向下的方法完整调用链文件名，使用"@"进行分隔后的列数，3列
    public static final int CALL_GRAPH_FILE_NAME_COLUMNS_3 = 3;

    // 方法调用表序号，代表非法的值
    public static final int METHOD_CALL_ID_ILLEGAL = -1;

    // 记录id，代表非法的值
    public static final int RECORD_ID_ILLEGAL = -1;

    // 方法完整调用链文件中的级别，代表起始的值
    public static final int CALL_GRAPH_METHOD_LEVEL_START = 0;

    // 方法完整调用链文件中的级别，代表非法的值
    public static final int CALL_GRAPH_METHOD_LEVEL_ILLEGAL = -1;

    // 向上的方法完整调用链文件，每行内容的最小列数
    public static final int CALL_GRAPH_EE_LINE_MIN_COLUMN_NUM = 1;

    // 向下的方法完整调用链文件，每行内容的最小列数
    public static final int CALL_GRAPH_ER_LINE_MIN_COLUMN_NUM = 1;

    // 输出的注解信息文件，包含属性时的列数
    public static final int ANNOTATION_COLUMN_NUM_WITH_ATTRIBUTE_5 = 5;
    // 输出的注解信息文件，不包含属性时的列数
    public static final int ANNOTATION_COLUMN_NUM_WITHOUT_ATTRIBUTE_2 = 2;

    // 输出的注解信息文件，包含属性时的列数
    public static final int ANNOTATION_COLUMN_NUM_WITH_ATTRIBUTE_6 = 6;
    // 输出的注解信息文件，不包含属性时的列数
    public static final int ANNOTATION_COLUMN_NUM_WITHOUT_ATTRIBUTE_3 = 3;

    // 输出的注解信息文件，多一列其他信息，包含属性时的列数
    public static final int ANNOTATION_COLUMN_NUM_WITH_ATTRIBUTE_7 = 7;
    // 输出的注解信息文件，多一列其他信息，不包含属性时的列数
    public static final int ANNOTATION_COLUMN_NUM_WITHOUT_ATTRIBUTE_4 = 4;

    public static final String H2_PROTOCOL = "jdbc:h2:file:";
    public static final String H2_SCHEMA = "jacg";
    public static final String H2_FILE_EXT = ".mv.db";

    public static final String THREAD_NAME_PREFIX_WORKER = "jacg_worker";

    // Spring事务类型，使用注解
    public static final String SPRING_TX_TYPE_ANNOTATION = "annotation";
    // Spring事务类型，使用事务模板
    public static final String SPRING_TX_TYPE_TEMPLATE = "template";

    // Spring Task类型，通过XML定义
    public static final String SPRING_TASK_TYPE_XML = "XML";
    // Spring Task类型，通过注解定义
    public static final String SPRING_TASK_TYPE_ANNOTATION = "annotation";

    // 代表类或方法为空的标志
    public static final String EMPTY_CLASS_METHOD = "#empty#";

    // 数据库表后缀，代表旧的
    public static final String TABLE_SUFFIX_OLD = "_1";
    // 数据库表后缀，代表新的
    public static final String TABLE_SUFFIX_NEW = "_2";

    // 类名占位符
    public static final String CLASS_PLACE_HOLDER = "@class_place_holder@";
    // 方法名占位符
    public static final String METHOD_PLACE_HOLDER = "@method_place_holder@";

    private JACGConstants() {
        throw new IllegalStateException("illegal");
    }
}
