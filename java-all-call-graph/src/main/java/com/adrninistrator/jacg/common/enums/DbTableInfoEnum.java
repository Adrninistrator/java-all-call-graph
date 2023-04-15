package com.adrninistrator.jacg.common.enums;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.JACGConstants;

/**
 * @author adrninistrator
 * @date 2022/11/16
 * @description: 数据库表信息枚举
 */
public enum DbTableInfoEnum {
    DTIE_ALLOWED_CLASS_PREFIX("allowed_class_prefix", new String[]{
            DC.ACP_RECORD_ID,
            DC.ACP_CLASS_PREFIX,
    }),
    DTIE_BUSINESS_DATA("business_data", new String[]{
            DC.BD_CALL_ID,
            DC.BD_DATA_TYPE,
            DC.BD_DATA_VALUE
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
    DTIE_CLASS_INFO("class_info", new String[]{
            DC.CI_RECORD_ID,
            DC.CI_SIMPLE_CLASS_NAME,
            DC.CI_ACCESS_FLAGS,
            DC.CI_CLASS_NAME
    }),
    DTIE_CLASS_NAME("class_name", new String[]{
            DC.CN_RECORD_ID,
            DC.CN_CLASS_NAME,
            DC.CN_SIMPLE_CLASS_NAME,
            DC.CN_DUPLICATE_CLASS
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
    DTIE_INNER_CLASS("inner_class", new String[]{
            DC.IC_SIMPLE_INNER_CLASS_NAME,
            DC.IC_INNER_CLASS_NAME,
            DC.IC_SIMPLE_OUTER_CLASS_NAME,
            DC.IC_OUTER_CLASS_NAME,
            DC.IC_ANONYMOUS_CLASS
    }),
    DTIE_JAR_INFO("jar_info", new String[]{
            DC.JI_JAR_NUM,
            DC.JI_JAR_TYPE,
            DC.JI_JAR_PATH_HASH,
            DC.JI_JAR_FULL_PATH,
            DC.JI_LAST_MODIFIED,
            DC.JI_JAR_HASH
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
    DTIE_METHOD_ARG_GENERICS_TYPE("method_arg_generics_type", new String[]{
            DC.MAGT_RECORD_ID,
            DC.MAGT_METHOD_HASH,
            DC.MAGT_SIMPLE_CLASS_NAME,
            DC.MAGT_ARG_SEQ,
            DC.MAGT_TYPE,
            DC.MAGT_TYPE_SEQ,
            DC.MAGT_SIMPLE_GENERICS_TYPE,
            DC.MAGT_GENERICS_TYPE,
            DC.MAGT_FULL_METHOD
    }),
    DTIE_METHOD_ARG_TYPE("method_arg_type", new String[]{
            DC.MAT_METHOD_HASH,
            DC.MAT_ARG_SEQ,
            DC.MAT_SIMPLE_ARG_TYPE,
            DC.MAT_ARG_TYPE,
            DC.MAT_SIMPLE_CLASS_NAME,
            DC.MAT_FULL_METHOD
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
    DTIE_METHOD_CALL_INFO("method_call_info", new String[]{
            DC.MCI_CALL_ID,
            DC.MCI_OBJ_ARGS_SEQ,
            DC.MCI_SEQ,
            DC.MCI_TYPE,
            DC.MCI_ARRAY_FLAG,
            DC.MCI_THE_VALUE
    }),
    DTIE_METHOD_INFO("method_info", new String[]{
            DC.MI_METHOD_HASH,
            DC.MI_SIMPLE_CLASS_NAME,
            DC.MI_ACCESS_FLAGS,
            DC.MI_METHOD_NAME,
            DC.MI_FULL_METHOD,
            DC.MI_SIMPLE_RETURN_TYPE,
            DC.MI_RETURN_TYPE
    }),
    DTIE_METHOD_LINE_NUMBER("method_line_number", new String[]{
            DC.MLN_METHOD_HASH,
            DC.MLN_SIMPLE_CLASS_NAME,
            DC.MLN_MIN_LINE_NUMBER,
            DC.MLN_MAX_LINE_NUMBER,
            DC.MLN_FULL_METHOD
    }),
    DTIE_METHOD_RETURN_GENERICS_TYPE("method_return_generics_type", new String[]{
            DC.MRGT_RECORD_ID,
            DC.MRGT_METHOD_HASH,
            DC.MRGT_SIMPLE_CLASS_NAME,
            DC.MRGT_TYPE,
            DC.MRGT_TYPE_SEQ,
            DC.MRGT_SIMPLE_GENERICS_TYPE,
            DC.MRGT_GENERICS_TYPE,
            DC.MRGT_FULL_METHOD
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
    DTIE_SPRING_BEAN("spring_bean", new String[]{
            DC.SPB_RECORD_ID,
            DC.SPB_SPRING_BEAN_NAME,
            DC.SPB_SEQ,
            DC.SPB_CLASS_NAME
    }),
    DTIE_SPRING_CONTROLLER("spring_controller", new String[]{
            DC.SPC_METHOD_HASH,
            DC.SPC_SEQ,
            DC.SPC_SHOW_URI,
            DC.SPC_CLASS_PATH,
            DC.SPC_METHOD_PATH,
            DC.SPC_ANNOTATION_ANNOTATION_NAME,
            DC.SPC_SIMPLE_CLASS_NAME,
            DC.SPC_FULL_METHOD
    }),
    DTIE_SPRING_TASK("spring_task", new String[]{
            DC.SPT_RECORD_ID,
            DC.SPT_SPRING_BEAN_NAME,
            DC.SPT_CLASS_NAME,
            DC.SPT_METHOD_NAME,
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
     * @return
     */
    public String getTableName() {
        return "jacg_" + tableNameKeyword + "_" + JACGConstants.APP_NAME_IN_SQL;
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
