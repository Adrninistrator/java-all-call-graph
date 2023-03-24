package com.adrninistrator.jacg.common.enums;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.JACGConstants;

/**
 * @author adrninistrator
 * @date 2022/11/16
 * @description: 数据库表信息枚举
 */
public enum DbTableInfoEnum {
    DTIE_CLASS_NAME("class_name", new String[]{
            DC.CN_RECORD_ID,
            DC.CN_CLASS_NAME,
            DC.CN_SIMPLE_CLASS_NAME
    }),
    DTIE_CLASS_ANNOTATION("class_annotation", new String[]{
            DC.CA_RECORD_ID,
            DC.CA_SIMPLE_CLASS_NAME,
            DC.CA_ANNOTATION_NAME,
            DC.CA_ATTRIBUTE_NAME,
            DC.CA_ATTRIBUTE_TYPE,
            DC.CA_ATTRIBUTE_VALUE,
            DC.CA_CLASS_NAME
    }),
    DTIE_METHOD_ANNOTATION("method_annotation", new String[]{
            DC.MA_RECORD_ID,
            DC.MA_METHOD_HASH,
            DC.MA_ANNOTATION_NAME,
            DC.MA_ATTRIBUTE_NAME,
            DC.MA_ATTRIBUTE_TYPE,
            DC.MA_ATTRIBUTE_VALUE,
            DC.MA_FULL_METHOD,
            DC.MA_SIMPLE_CLASS_NAME,
            DC.MA_SPRING_MAPPING_ANNOTATION
    }),
    DTIE_METHOD_CALL("method_call", new String[]{
            DC.MC_CALL_ID,
            DC.MC_CALL_TYPE,
            DC.MC_CALLEE_OBJ_TYPE,
            DC.MC_ENABLED,
            DC.MC_CALLER_JAR_NUM,
            DC.MC_CALLER_METHOD_HASH,
            DC.MC_CALLER_SIMPLE_CLASS_NAME,
            DC.MC_CALLER_METHOD_NAME,
            DC.MC_CALLER_FULL_METHOD,
            DC.MC_CALLER_LINE_NUMBER,
            DC.MC_CALLEE_METHOD_HASH,
            DC.MC_CALLEE_SIMPLE_CLASS_NAME,
            DC.MC_CALLEE_METHOD_NAME,
            DC.MC_CALLEE_FULL_METHOD,
            DC.MC_CALL_FLAGS,
            DC.MC_RAW_RETURN_TYPE,
            DC.MC_ACTUAL_RETURN_TYPE
    }),
    DTIE_LAMBDA_METHOD_INFO("lambda_method_info", new String[]{
            DC.LMI_CALL_ID,
            DC.LMI_LAMBDA_CALLEE_CLASS_NAME,
            DC.LMI_LAMBDA_CALLEE_METHOD_NAME,
            DC.LMI_LAMBDA_CALLEE_FULL_METHOD,
            DC.LMI_LAMBDA_NEXT_CLASS_NAME,
            DC.LMI_LAMBDA_NEXT_METHOD_NAME,
            DC.LMI_LAMBDA_NEXT_FULL_METHOD,
            DC.LMI_LAMBDA_NEXT_IS_STREAM,
            DC.LMI_LAMBDA_NEXT_IS_INTERMEDIATE,
            DC.LMI_LAMBDA_NEXT_IS_TERMINAL
    }),
    DTIE_METHOD_LINE_NUMBER("method_line_number", new String[]{
            DC.MLN_METHOD_HASH,
            DC.MLN_SIMPLE_CLASS_NAME,
            DC.MLN_MIN_LINE_NUMBER,
            DC.MLN_MAX_LINE_NUMBER,
            DC.MLN_FULL_METHOD
    }),
    DTIE_JAR_INFO("jar_info", new String[]{
            DC.JI_JAR_NUM,
            DC.JI_JAR_TYPE,
            DC.JI_JAR_PATH_HASH,
            DC.JI_JAR_FULL_PATH,
            DC.JI_LAST_MODIFIED,
            DC.JI_JAR_HASH
    }),
    DTIE_BUSINESS_DATA("business_data", new String[]{
            DC.BD_CALL_ID,
            DC.BD_DATA_TYPE,
            DC.BD_DATA_VALUE
    }),
    DTIE_CLASS_INFO("class_info", new String[]{
            DC.CI_RECORD_ID,
            DC.CI_SIMPLE_CLASS_NAME,
            DC.CI_ACCESS_FLAGS,
            DC.CI_CLASS_NAME
    }),
    DTIE_METHOD_INFO("method_info", new String[]{
            DC.MI_METHOD_HASH,
            DC.MI_SIMPLE_CLASS_NAME,
            DC.MI_ACCESS_FLAGS,
            DC.MI_METHOD_NAME,
            DC.MI_FULL_METHOD
    }),
    DTIE_EXTENDS_IMPL("extends_impl", new String[]{
            DC.EI_RECORD_ID,
            DC.EI_SIMPLE_CLASS_NAME,
            DC.EI_CLASS_NAME,
            DC.EI_ACCESS_FLAGS,
            DC.EI_TYPE,
            DC.EI_SEQ,
            DC.EI_EXISTS_DOWNWARD_CLASSES,
            DC.EI_UPWARD_SIMPLE_CLASS_NAME,
            DC.EI_UPWARD_CLASS_NAME
    }),
    DTIE_METHOD_CALL_INFO("method_call_info", new String[]{
            DC.MCI_CALL_ID,
            DC.MCI_OBJ_ARGS_SEQ,
            DC.MCI_SEQ,
            DC.MCI_TYPE,
            DC.MCI_ARRAY_FLAG,
            DC.MCI_THE_VALUE
    }),
    DTIE_SPRING_BEAN("spring_bean", new String[]{
            DC.SB_RECORD_ID,
            DC.SB_SPRING_BEAN_NAME,
            DC.SB_SEQ,
            DC.SB_CLASS_NAME
    }),
    DTIE_SPRING_CONTROLLER("spring_controller", new String[]{
            DC.SC_METHOD_HASH,
            DC.SC_SEQ,
            DC.SC_SHOW_URI,
            DC.SC_CLASS_PATH,
            DC.SC_METHOD_PATH,
            DC.SC_ANNOTATION_ANNOTATION_NAME,
            DC.SC_SIMPLE_CLASS_NAME,
            DC.SC_FULL_METHOD
    }),
    DTIE_SPRING_TASK("spring_task", new String[]{
            DC.ST_RECORD_ID,
            DC.ST_SPRING_BEAN_NAME,
            DC.ST_CLASS_NAME,
            DC.ST_METHOD_NAME,
    }),
    DTIE_CLASS_SIGNATURE_EI1("class_signature_ei1", new String[]{
            DC.CSEI1_RECORD_ID,
            DC.CSEI1_SIMPLE_CLASS_NAME,
            DC.CSEI1_TYPE,
            DC.CSEI1_SUPER_ITF_CLASS_NAME,
            DC.CSEI1_SEQ,
            DC.CSEI1_SIGN_CLASS_NAME,
            DC.CSEI1_CLASS_NAME
    }),
    DTIE_MYBATIS_MS_TABLE("mybatis_ms_table", new String[]{
            DC.MMT_RECORD_ID,
            DC.MMT_MAPPER_SIMPLE_CLASS_NAME,
            DC.MMT_MAPPER_METHOD_NAME,
            DC.MMT_SQL_STATEMENT,
            DC.MMT_TABLE_SEQ,
            DC.MMT_TABLE_NAME,
            DC.MMT_MAPPER_CLASS_NAME
    }),
    DTIE_MYBATIS_MS_WRITE_TABLE("mybatis_ms_write_table", new String[]{
            DC.MMWT_RECORD_ID,
            DC.MMWT_MAPPER_SIMPLE_CLASS_NAME,
            DC.MMWT_MAPPER_METHOD_NAME,
            DC.MMWT_SQL_STATEMENT,
            DC.MMWT_TABLE_NAME,
            DC.MMWT_MAPPER_CLASS_NAME
    }),
    DTIE_ALLOWED_CLASS_PREFIX("allowed_class_prefix", new String[]{
            DC.ACP_RECORD_ID,
            DC.ACP_CLASS_PREFIX,
    }),
    DTIE_METHOD_ARG_GENERICS_TYPE("method_arg_generics_type", new String[]{
            DC.MAGT_RECORD_ID,
            DC.MAGT_METHOD_HASH,
            DC.MAGT_SIMPLE_CLASS_NAME,
            DC.MAGT_ARG_SEQ,
            DC.MAGT_TYPE,
            DC.MAGT_TYPE_SEQ,
            DC.MAGT_GENERICS_TYPE,
            DC.MAGT_FULL_METHOD
    }),
    ;

    private final String tableNameKeyword;

    private final String[] columns;

    DbTableInfoEnum(String tableNameKeyword, String[] columns) {
        this.tableNameKeyword = tableNameKeyword;
        this.columns = columns;
    }

    /**
     * 获取sql语句缓存key
     *
     * @return
     */
    public String getSqlKey() {
        return "insert_" + ordinal();
    }

    /**
     * 获取sql语句缓存key，用于在日志中打印
     *
     * @return
     */
    public String getSqlKey4Print() {
        return tableNameKeyword;
    }

    /**
     * 获取表名，固定使用前缀"jacg_"，使用配置参数中的appName作为后缀
     *
     * @param appName
     * @return
     */
    public String getTableName(String appName) {
        return "jacg_" + tableNameKeyword + "_" + appName;
    }

    /**
     * 获取数据库表对应的sql文件名
     *
     * @return
     */
    public String getTableFileName() {
        return tableNameKeyword + JACGConstants.EXT_SQL;
    }

    /**
     * 获取数据库表的列名
     *
     * @return
     */
    public String[] getColumns() {
        return columns;
    }
}
