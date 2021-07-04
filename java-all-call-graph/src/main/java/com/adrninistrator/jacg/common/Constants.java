package com.adrninistrator.jacg.common;

/**
 * @author adrninistrator
 * @date 2021/6/17
 * @description:
 */

public class Constants {

    public static final String[] TABLE_COLUMNS_CLASS_NAME = new String[]{
            DC.CN_FULL_NAME,
            DC.CN_SIMPLE_NAME};

    public static final String[] TABLE_COLUMNS_METHOD_ANNOTATION = new String[]{
            DC.MA_METHOD_HASH,
            DC.MA_ANNOTATION_NAME,
            DC.MA_FULL_METHOD,
    };

    public static final String[] TABLE_COLUMNS_METHOD_CALL = new String[]{
            DC.MC_ID,
            DC.MC_CALL_TYPE,
            DC.MC_CALLER_METHOD_HASH,
            DC.MC_CALLER_FULL_METHOD,
            DC.MC_CALLER_METHOD_NAME,
            DC.MC_CALLER_FULL_CLASS_NAME,
            DC.MC_CALLER_CLASS_NAME,
            DC.MC_CALLEE_METHOD_HASH,
            DC.MC_CALLEE_FULL_METHOD,
            DC.MC_CALLEE_METHOD_NAME,
            DC.MC_CALLEE_FULL_CLASS_NAME,
            DC.MC_CALLEE_CLASS_NAME
    };

    public static final String DIR_CONFIG = "~jacg_config";
    public static final String DIR_OUTPUT_GRAPH_FOR_CALLEE = "~jacg_output_for_callee";
    public static final String DIR_OUTPUT_GRAPH_FOR_CALLER = "~jacg_output_for_caller";
    public static final String DIR_SQL = "~jacg_sql";

    public static final String FILE_CONFIG = "config.properties";

    public static final String FILE_IN_ALLOWED_CLASS_PREFIX = "i_allowed_class_prefix.properties";
    public static final String FILE_IN_ANNOTATION_TAIL = "-annotation.txt";

    public static final String FILE_OUT_GRAPH_FOR_CALLEE_CLASS_NAME = "o_g4callee_class_name.properties";
    public static final String FILE_OUT_GRAPH_FOR_CALLER_ENTRY_METHOD = "o_g4caller_entry_method.properties";
    public static final String FILE_OUT_GRAPH_FOR_CALLER_ENTRY_METHOD_IGNORE_PREFIX = "o_g4caller_entry_method_ignore_prefix.properties";
    public static final String FILE_OUT_GRAPH_FOR_CALLER_IGNORE_CLASS_KEYWORD = "o_g4caller_ignore_class_keyword.properties";
    public static final String FILE_OUT_GRAPH_FOR_CALLER_IGNORE_FULL_METHOD_PREFIX = "o_g4caller_ignore_full_method_prefix.properties";
    public static final String FILE_OUT_GRAPH_FOR_CALLER_IGNORE_METHOD_PREFIX = "o_g4caller_ignore_method_prefix.properties";

    public static final String FILE_SQL_CLASS_NAME = "class_name.sql";
    public static final String FILE_SQL_METHOD_ANNOTATION = "method_annotation.sql";
    public static final String FILE_SQL_METHOD_CALL = "method_call.sql";


    public static final String APPNAME_IN_SQL = "{appName}";

    public static final String KEY_APPNAME = "app.name";
    public static final String KEY_CALL_GRAPH_JAR_LIST = "call.graph.jar.list";
    public static final String KEY_INPUT_IGNORE_OTHER_PACKAGE = "input.ignore.other.package";
    public static final String KEY_CALL_GRAPH_OUTPUT_DETAIL = "call.graph.output.detail";
    public static final String KEY_THREAD_NUM = "thread.num";
    public static final String KEY_SHOW_METHOD_ANNOTATION = "show.method.annotation";
    public static final String KEY_GEN_COMBINED_OUTPUT = "gen.combined.output";

    public static final String KEY_DB_DRIVER_NAME = "db.driver.name";
    public static final String KEY_DB_URL = "db.url";
    public static final String KEY_DB_USERNAME = "db.username";
    public static final String KEY_DB_PASSWORD = "db.password";

    public static final String CONFIG_OUTPUT_DETAIL_1 = "1";
    public static final String CONFIG_OUTPUT_DETAIL_2 = "2";
    public static final String CONFIG_OUTPUT_DETAIL_3 = "3";

    public static final String FILE_KEY_CLASS_PREFIX = "C:";
    public static final String FILE_KEY_METHOD_PREFIX = "M:";

    // 以上开头字符串长度
    public static final int FILE_KEY_PREFIX_LENGTH = 2;

    public static final String SQL_KEY_CN_QUERY_DUPLICATE_CLASS = "cn_query_duplicate_class";
    public static final String SQL_KEY_CN_QUERY_SIMPLE_CLASS = "cn_query_simple_class";
    public static final String SQL_KEY_CN_QUERY_FULL_CLASS = "cn_query_full_class";

    public static final String SQL_KEY_MA_QUERY_METHOD_ANNOTATION = "ma_query_method_annotation";

    public static final String SQL_KEY_MC_QUERY_CLASS_EXISTS = "mc_query_class_exists";
    public static final String SQL_KEY_MC_QUERY_CALLER_FULL_CLASS = "mc_query_caller_full_class";
    public static final String SQL_KEY_MC_QUERY_TOP_METHOD = "mc_query_top_method";
    public static final String SQL_KEY_MC_QUERY_ONE_CALLEE = "mc_query_one_callee";
    public static final String SQL_KEY_MC_QUERY_CALLEE_ALL_METHODS = "mc_query_callee_all_methods";
    public static final String SQL_KEY_MC_QUERY_ONE_CALLER1 = "mc_query_one_caller1";
    public static final String SQL_KEY_MC_QUERY_ONE_CALLER2 = "mc_query_one_caller2";

    public static final String SQL_KEY_INSERT_CLASS_NAME = "insert_class_name";
    public static final String SQL_KEY_INSERT_METHOD_ANNOTATION = "insert_method_annotation";
    public static final String SQL_KEY_INSERT_METHOD_CALL = "insert_method_call";

    public static final String FLAG_DOT = ".";
    public static final String FLAG_COLON = ":";
    public static final String FLAG_LEFT_BRACKET = "(";
    public static final String FLAG_RIGHT_BRACKET = ")";
    public static final String FLAG_SPACE = " ";
    public static final String FLAG_HASHTAG = "#";
    public static final String FLAG_AT = "@";
    public static final String FLAG_COMMA_WITH_SPACE = ", ";

    public static final String EXT_TXT = ".txt";

    public static final String NEW_LINE = "\n";

    public static final int BATCH_SIZE = 1000;
    public static final int MAX_THREAD_NUM = 100;
    public static final int NOTICE_LINE_NUM = 500;

    public static final String TABLE_PREFIX_CLASS_NAME = "class_name_";
    public static final String TABLE_PREFIX_METHOD_ANNOTATION = "method_annotation_";
    public static final String TABLE_PREFIX_METHOD_CALL = "method_call_";

    public static final String OUTPUT_SPLIT_FLAG = "  ";

    public static final String COMBINE_FILE_NAME_PREFIX = "~all";
    public static final String COMBINE_FILE_NAME_4_CALLEE = "-4callee";
    public static final String COMBINE_FILE_NAME_4_CALLER = "-4caller";

    public static final int THREAD_POOL_MAX_QUEUE_SIZE = 100;
    public static final int THREAD_POOL_WAIT_QUEUE_SIZE = 80;

    public static final String CALLEE_FLAG_ENTRY = "\t!entry!";
    public static final String CALL_FLAG_CYCLE = "\t!cycle[%d]!";

    public static final String JAVA_CALL_GRAPH_FLAG_OUT_FILE = "output.file";

    public static final String MYSQL_FLAG = "mysql";
    public static final String MYSQL_REWRITEBATCHEDSTATEMENTS = "rewriteBatchedStatements=true";

    public static final int METHOD_CALL_ID_START = 0;

    public static final int NO_CYCLE_CALL_FLAG = -1;

    public static final String CALL_TYPE_RUNNABLE_INIT_RUN = "RIR";
    public static final String CALL_TYPE_THREAD_INIT_RUN = "TIR";

    private Constants() {
        throw new IllegalStateException("illegal");
    }
}
