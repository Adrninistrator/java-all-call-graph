package com.adrninistrator.jacg.common;

/**
 * @author adrninistrator
 * @date 2021/6/17
 * @description:
 */

public class JACGConstants {

    public static final String[] TABLE_COLUMNS_CLASS_NAME = new String[]{
            DC.CN_FULL_NAME,
            DC.CN_SIMPLE_NAME
    };

    public static final String[] TABLE_COLUMNS_CLASS_ANNOTATION = new String[]{
            DC.CA_FULL_CLASS_NAME,
            DC.CA_ANNOTATION_NAME,
            DC.CA_ATTRIBUTE_NAME,
            DC.CA_ATTRIBUTE_VALUE
    };

    public static final String[] TABLE_COLUMNS_METHOD_ANNOTATION = new String[]{
            DC.MA_METHOD_HASH,
            DC.MA_ANNOTATION_NAME,
            DC.MA_ATTRIBUTE_NAME,
            DC.MA_ATTRIBUTE_VALUE,
            DC.MA_FULL_METHOD
    };

    public static final String[] TABLE_COLUMNS_METHOD_CALL = new String[]{
            DC.MC_CALL_ID,
            DC.MC_CALL_TYPE,
            DC.MC_ENABLED,
            DC.MC_CALLER_JAR_NUM,
            DC.MC_CALLER_METHOD_HASH,
            DC.MC_CALLER_FULL_METHOD,
            DC.MC_CALLER_METHOD_NAME,
            DC.MC_CALLER_FULL_CLASS_NAME,
            DC.MC_CALLER_CLASS_NAME,
            DC.MC_CALLER_LINE_NUM,
            DC.MC_CALLEE_METHOD_HASH,
            DC.MC_CALLEE_FULL_METHOD,
            DC.MC_CALLEE_METHOD_NAME,
            DC.MC_CALLEE_FULL_CLASS_NAME,
            DC.MC_CALLEE_CLASS_NAME
    };

    public static final String[] TABLE_COLUMNS_METHOD_LINE_NUMBER = new String[]{
            DC.MLN_METHOD_HASH,
            DC.MLN_SIMPLE_CLASS_NAME,
            DC.MLN_MIN_LINE_NUMBER,
            DC.MLN_MAX_LINE_NUMBER,
            DC.MLN_FULL_METHOD
    };

    public static final String[] TABLE_COLUMNS_JAR_INFO = new String[]{
            DC.JI_JAR_NUM,
            DC.JI_JAR_TYPE,
            DC.JI_JAR_PATH_HASH,
            DC.JI_JAR_FULL_PATH,
            DC.JI_LAST_MODIFIED,
            DC.JI_JAR_HASH
    };

    public static final String[] TABLE_COLUMNS_EXTENDED_DATA = new String[]{
            DC.ED_CALL_ID,
            DC.ED_DATA_TYPE,
            DC.ED_DATA_VALUE
    };

    public static final String[] TABLE_COLUMNS_MANUAL_ADD_EXTENDED_DATA = new String[]{
            DC.MAED_CALLER_FULL_METHOD,
            DC.MAED_CALLEE_FULL_METHOD,
            DC.MAED_CALLEE_SEQ_IN_CALLER,
            DC.MAED_DATA_TYPE,
            DC.MAED_DATA_VALUE
    };

    public static final String DIR_OUTPUT_GRAPH_FOR_CALLEE = "_jacg_o_ee";
    public static final String DIR_OUTPUT_GRAPH_FOR_CALLER = "_jacg_o_er";
    public static final String DIR_OUTPUT_METHODS = "methods";
    public static final String DIR_OUTPUT_FIND_KEYWORD = "_find_kw";

    public static final String FILE_CONFIG = "config.properties";

    public static final String FILE_SQL_CLASS_NAME = "class_name.sql";
    public static final String FILE_SQL_CLASS_ANNOTATION = "class_annotation.sql";
    public static final String FILE_SQL_METHOD_ANNOTATION = "method_annotation.sql";
    public static final String FILE_SQL_METHOD_CALL = "method_call.sql";
    public static final String FILE_SQL_METHOD_LINE_NUMBER = "method_line_number.sql";
    public static final String FILE_SQL_JAR_INFO = "jar_info.sql";
    public static final String FILE_SQL_EXTENDED_DATA = "extended_data.sql";
    public static final String FILE_SQL_MANUAL_ADD_EXTENDED_DATA = "manual_add_extended_data.sql";

    public static final String FILE_MAPPING_NAME = "_mapping.txt";

    public static final String APPNAME_IN_SQL = "{appName}";

    // 以上开头字符串长度
    public static final int FILE_KEY_PREFIX_LENGTH = 2;

    public static final String SQL_KEY_CN_QUERY_DUPLICATE_CLASS = "cn_query_duplicate_class";
    public static final String SQL_KEY_CN_QUERY_SIMPLE_CLASS = "cn_query_simple_class";
    public static final String SQL_KEY_CN_QUERY_FULL_CLASS = "cn_query_full_class";
    public static final String SQL_KEY_CN_UPDATE_SIMPLE_2_FULL = "cn_update_simple_2_full";

    public static final String SQL_KEY_MC_QUERY_CALLER_FULL_CLASS = "mc_query_caller_full_class";
    public static final String SQL_KEY_MC_QUERY_TOP_METHOD = "mc_query_top_method";
    public static final String SQL_KEY_MC_QUERY_ONE_CALLEE = "mc_query_one_callee";
    public static final String SQL_KEY_MC_QUERY_ONE_CALLEE_CHECK_LINE_NUM = "mc_query_one_callee_cln";
    public static final String SQL_KEY_MC_QUERY_CALLEE_ALL_METHODS = "mc_query_callee_all_methods";
    public static final String SQL_KEY_MC_QUERY_CALLER_ALL_METHODS = "mc_query_caller_all_methods";
    public static final String SQL_KEY_MC_QUERY_ONE_CALLER1 = "mc_query_one_caller1";
    public static final String SQL_KEY_MC_QUERY_ONE_CALLER2 = "mc_query_one_caller2";
    public static final String SQL_KEY_MC_QUERY_NOTICE_INFO = "mc_query_notice_info";
    public static final String SQL_KEY_MC_QUERY_ALL_CALLER = "mc_query_all_caller";
    public static final String SQL_KEY_MC_QUERY_CALLEE_SEQ_IN_CALLER = "mc_query_callee_in_caller_seq";
    public static final String SQL_KEY_MC_QUERY_CALLEE_BY_ID = "mc_query_callee_by_id";
    public static final String SQL_KEY_MC_QUERY_IMPL_METHODS = "mc_query_impl_methods";
    public static final String SQL_KEY_MC_QUERY_CALLER_FULL_METHOD_BY_HASH = "mc_query_erfm_by_hash";
    public static final String SQL_KEY_MC_QUERY_CALLEE_FULL_METHOD_BY_HASH = "mc_query_eefm_by_hash";
    public static final String SQL_KEY_MC_QUERY_MAX_CALL_ID = "mc_query_max_call_id";
    public static final String SQL_KEY_MC_QUERY_ERFM_BY_EEFM = "mc_query_erfm_by_eefm";
    public static final String SQL_KEY_MC_QUERY_ERFM_BY_EEFM_LIKE_PREFIX = "mc_query_erfm_by_eefm_LIKE_PREFIX";
    public static final String SQL_KEY_MC_QUERY_CHECK_NORMAL_MC_BY_EE_HASH = "mc_query_check_normal_mc_by_ee_hash";

    public static final String SQL_KEY_MLN_QUERY_METHOD = "mln_query_method";

    public static final String SQL_KEY_JI_QUERY_JAR_INFO = "ji_query_jar_info";

    public static final String SQL_KEY_MA_QUERY_FMAH_WITH_ANNOTATIONS = "ma_query_fmah_with_annotations";
    public static final String SQL_KEY_MA_QUERY_FMAH_WITH_ANNOTATIONS_OF_CLASS = "ma_query_fmah_with_annotations_of_class";
    public static final String SQL_KEY_MA_QUERY_FULL_METHOD_WITH_ANNOTATIONS = "ma_query_full_method_with_annotations";

    public static final String SQL_KEY_CA_QUERY_FULL_CLASS_NAME_WITH_ANNOTATION = "ca_query_full_class_name_with_annotation";

    public static final String SQL_KEY_ED_QUERY_EXTENDED_DATA = "ed_query_extended_data";

    public static final String SQL_KEY_MAED_QUERY = "ed_query_MAED";
    public static final String SQL_KEY_MAED_QUERY_IGNORE_CALLER = "ed_query_MAED_ignore_caller";

    public static final String SQL_KEY_INSERT_CLASS_NAME = "insert_class_name";
    public static final String SQL_KEY_INSERT_CLASS_ANNOTATION = "insert_class_annotation";
    public static final String SQL_KEY_INSERT_METHOD_ANNOTATION = "insert_method_annotation";
    public static final String SQL_KEY_INSERT_METHOD_CALL = "insert_method_call";
    public static final String SQL_KEY_INSERT_METHOD_LINE_NUMBER = "insert_method_line_number";
    public static final String SQL_KEY_INSERT_JAR_INFO = "insert_jar_info";
    public static final String SQL_KEY_INSERT_EXTENDED_DATA = "insert_extended_data";

    public static final String SQL_VALUE_MAED_CALLER_FULL_METHOD_ALL = "*";

    public static final String SQL_CREATE_TABLE_HEAD = "CREATE TABLE if not exists";
    public static final int SQL_CREATE_TABLE_HEAD_LENGTH = SQL_CREATE_TABLE_HEAD.length();

    public static final String FLAG_DOT = ".";
    public static final String FLAG_COLON = ":";
    public static final String FLAG_LEFT_BRACKET = "(";
    public static final String FLAG_RIGHT_BRACKET = ")";
    public static final String FLAG_LEFT_PARENTHESES = "[";
    public static final String FLAG_RIGHT_PARENTHESES = "]";
    public static final String FLAG_LEFT_BIG_PARENTHESES = "{";
    public static final String FLAG_RIGHT_BIG_PARENTHESES = "}";
    public static final String FLAG_SPACE = " ";
    public static final String FLAG_HASHTAG = "#";
    public static final String FLAG_AT = "@";
    public static final String FLAG_MINUS = "-";
    public static final String FLAG_UNDER_LINE = "_";
    public static final String FLAG_TAB = "\t";
    public static final String FLAG_COMMA_WITH_SPACE = ", ";

    public static final char FLAG_CHAR_SPACE = FLAG_SPACE.charAt(0);

    public static final int FLAG_LEFT_PARENTHESES_LENGTH = FLAG_LEFT_PARENTHESES.length();

    public static final String FLAG_MD_CODE = "```";
    public static final String FLAG_MD_CODE_SQL = FLAG_MD_CODE + "sql";
    public static final String FLAG_MD_LINE_NUMBER = " 行号: ";

    public static final String FLAG_EMPTY = "-empty";

    public static final String EXT_TXT = ".txt";
    public static final String EXT_EMPTY_TXT = FLAG_EMPTY + EXT_TXT;
    public static final String EXT_MD = ".md";
    public static final String EXT_EMPTY_MD = FLAG_EMPTY + EXT_MD;

    public static final String NEW_LINE = "\n";

    public static final String TABLE_PREFIX_CLASS_NAME = "class_name_";
    public static final String TABLE_PREFIX_METHOD_ANNOTATION = "method_annotation_";
    public static final String TABLE_PREFIX_CLASS_ANNOTATION = "class_annotation_";
    public static final String TABLE_PREFIX_METHOD_CALL = "method_call_";
    public static final String TABLE_PREFIX_METHOD_LINE_NUMBER = "method_line_number_";
    public static final String TABLE_PREFIX_JAR_INFO = "jar_info_";
    public static final String TABLE_PREFIX_EXTENDED_DATA = "extended_data_";
    public static final String TABLE_PREFIX_MANUAL_ADD_EXTENDED_DATA = "manual_add_extended_data_";

    // 调用链文件中，每个级别之间的缩进，两个空格
    public static final String OUTPUT_SPLIT_FLAG = FLAG_SPACE + FLAG_SPACE;

    public static final String COMBINE_FILE_NAME_PREFIX = "_all";
    public static final String COMBINE_FILE_NAME_4_CALLEE = "-4callee";
    public static final String COMBINE_FILE_NAME_4_CALLER = "-4caller";

    public static final String CALLEE_FLAG_ENTRY = FLAG_TAB + "!entry!";
    public static final String CALL_FLAG_CYCLE = FLAG_TAB + "!cycle[%d]!";
    public static final String CALL_FLAG_EXTENDED_DATA_NO_TAB = "!ext_data!";
    public static final String CALL_FLAG_EXTENDED_DATA_MANUAL_ADD_NO_TAB = "!ext_data_ma!";
    public static final String CALL_FLAG_EXTENDED_DATA = FLAG_TAB + CALL_FLAG_EXTENDED_DATA_NO_TAB;
    public static final String CALL_FLAG_EXTENDED_DATA_MANUAL_ADD = FLAG_TAB + CALL_FLAG_EXTENDED_DATA_MANUAL_ADD_NO_TAB;

    public static final String MYSQL_FLAG = "mysql";
    public static final String MYSQL_REWRITEBATCHEDSTATEMENTS = "rewriteBatchedStatements=true";

    public static final int METHOD_CALL_ID_START = 0;

    public static final int NO_CYCLE_CALL_FLAG = -1;

    // 保存配置文件的根目录
    public static final String PROPERTY_INPUT_ROOT_PATH = "input.root.path";
    // 是否在结果文件中写入配置信息
    public static final String PROPERTY_WRITE_CONFIG_IN_RESULT = "write.config";
    // 生成结果文件根目录
    public static final String PROPERTY_OUTPUT_ROOT_PATH = "output.root.path";
    // 跳过检查Jar包文件是否有更新
    public static final String PROPERTY_SKIP_CHECK_JAR_FILE_UPDATED = "skip.check.jar.file.updated";
    // 指定批量写入数据库时每次插入的数量
    public static final String PROPERTY_DB_INSERT_BATCH_SIZE = "db.insert.batch.size";

    public static final int DB_INSERT_BATCH_SIZE = System.getProperty(PROPERTY_DB_INSERT_BATCH_SIZE) != null ?
            Integer.parseInt(System.getProperty(PROPERTY_DB_INSERT_BATCH_SIZE)) : 1000;
    public static final int MAX_THREAD_NUM = 100;
    public static final int NOTICE_LINE_NUM = 5000;
    public static final int DB_TEXT_MAX_CHARACTER_SIZE = 3000;

    public static final int ENABLED = 1;
    public static final int DISABLED = 0;

    public static final String NOTICE_MULTI_ITF_MD = "_notice_multi_ITF.md";
    public static final String NOTICE_MULTI_SCC_MD = "_notice_multi_SCC.md";
    public static final String NOTICE_DISABLED_ITF_MD = "_notice_disabled_ITF.md";
    public static final String NOTICE_DISABLED_SCC_MD = "_notice_disabled_SCC.md";

    public static final int LINE_NUM_NONE = -1;
    // 调用链搜索文件文件中，代表未处理数据序号值
    public static final int DATA_SEQ_NONE = -1;

    // 向下的方法完整调用链文件名，使用“@”进行分隔后最小的列数
    public static final int CALLER_FILE_NAME_SPLIT_BY_AT_MIN_COLUMNS = 3;

    // 方法调用表最大序号，代表非法的值
    public static final int MAX_METHOD_CALL_ID_ILLEGAL = -1;

    // 方法完整调用链文件中的级别，代表起始的值
    public static final int CALL_GRAPH_METHOD_LEVEL_START = 0;

    // 方法完整调用链文件中的级别，代表非法的值
    public static final int CALL_GRAPH_METHOD_LEVEL_ILLEGAL = -1;

    public static final String DATA_TYPE_JUMP_MULTI_IMPL = "JUMP_MULTI_IMPL";

    public static final String H2_PROTOCOL = "jdbc:h2:file:";
    public static final String H2_SCHEMA = "jacg";
    public static final String H2_FILE_EXT = ".mv.db";

    public static final String JAR_TYPE_JAR = "jar";
    public static final String JAR_TYPE_DIR = "dir";

    public static final String KEYWORDS_NOT_FOUND_DIR = "_keywords_not_found";

    private JACGConstants() {
        throw new IllegalStateException("illegal");
    }
}
