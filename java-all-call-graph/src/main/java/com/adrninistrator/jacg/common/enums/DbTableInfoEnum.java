package com.adrninistrator.jacg.common.enums;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.JACGConstants;

/**
 * @author adrninistrator
 * @date 2022/11/16
 * @description: 数据库表信息枚举
 */
public enum DbTableInfoEnum {
    DTIE_JAVACG2_CONFIG("javacg2_config", new String[]{
            DC.JAVACG2C_CONFIG_FILE_NAME,
            DC.JAVACG2C_CONFIG_KEY,
            DC.JAVACG2C_CONFIG_VALUE,
            DC.JAVACG2C_CONFIG_TYPE,
    }),
    DTIE_ALLOWED_CLASS_PREFIX("allowed_class_prefix", new String[]{
            DC.ACP_RECORD_ID,
            DC.ACP_CLASS_PREFIX,
    }),
    DTIE_BUSINESS_DATA("business_data", new String[]{
            DC.BD_CALL_ID,
            DC.BD_DATA_TYPE,
            DC.BD_DATA_VALUE,
    }),
    DTIE_CLASS_ANNOTATION("class_annotation", new String[]{
            DC.CA_RECORD_ID,
            DC.CA_SIMPLE_CLASS_NAME,
            DC.CA_ANNOTATION_NAME,
            DC.CA_ATTRIBUTE_NAME,
            DC.CA_ATTRIBUTE_TYPE,
            DC.CA_ATTRIBUTE_VALUE,
            DC.CA_CLASS_NAME,
    }),
    DTIE_CLASS_INFO("class_info", new String[]{
            DC.CI_RECORD_ID,
            DC.CI_SIMPLE_CLASS_NAME,
            DC.CI_ACCESS_FLAGS,
            DC.CI_CLASS_NAME,
            DC.CI_CLASS_FILE_HASH,
            DC.CI_JAR_NUM,
    }),
    DTIE_CLASS_NAME("class_name", new String[]{
            DC.CN_RECORD_ID,
            DC.CN_CLASS_NAME,
            DC.CN_SIMPLE_CLASS_NAME,
            DC.CN_DUPLICATE_CLASS,
    }),
    DTIE_CLASS_REFERENCE("class_reference", new String[]{
            DC.CR_RECORD_ID,
            DC.CR_CLASS_NAME,
            DC.CR_SIMPLE_CLASS_NAME,
            DC.CR_REFERENCED_CLASS_NAME,
            DC.CR_REFERENCED_SIMPLE_CLASS_NAME,
    }),
    DTIE_CLASS_SIGNATURE_GENERICS_TYPE("class_signature_generics_type", new String[]{
            DC.CSGT_RECORD_ID,
            DC.CSGT_SIMPLE_CLASS_NAME,
            DC.CSGT_SEQ,
            DC.CSGT_TYPE_VARIABLES_NAME,
            DC.CSGT_GENERICS_EXTENDS_CLASS_NAME,
            DC.CSGT_CLASS_NAME,
    }),
    DTIE_CLASS_EXT_IMPL_GENERICS_TYPE("class_ext_impl_generics_type", new String[]{
            DC.CEIGT_RECORD_ID,
            DC.CEIGT_SIMPLE_CLASS_NAME,
            DC.CEIGT_EXT_TYPE,
            DC.CEIGT_SEQ,
            DC.CEIGT_SUPER_ITF_SIMPLE_CLASS_NAME,
            DC.CEIGT_GENERICS_SEQ,
            DC.CEIGT_SIMPLE_GENERICS_TYPE,
            DC.CEIGT_GENERICS_ARRAY_DIMENSIONS,
            DC.CEIGT_TYPE_VARIABLES_NAME,
            DC.CEIGT_GENERICS_CATEGORY,
            DC.CEIGT_GENERICS_TYPE,
            DC.CEIGT_CLASS_NAME,
            DC.CEIGT_SUPER_ITF_CLASS_NAME,
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
            DC.EI_UPWARD_CLASS_NAME,
    }),
    DTIE_EXTENDS_IMPL_PRE("extends_impl!pre", null),
    DTIE_INNER_CLASS("inner_class", new String[]{
            DC.IC_INNER_SIMPLE_CLASS_NAME,
            DC.IC_INNER_CLASS_NAME,
            DC.IC_OUTER_SIMPLE_CLASS_NAME,
            DC.IC_OUTER_CLASS_NAME,
            DC.IC_ANONYMOUS_CLASS,
    }),
    DTIE_JAR_INFO("jar_info", new String[]{
            DC.JI_JAR_NUM,
            DC.JI_JAR_TYPE,
            DC.JI_JAR_PATH_HASH,
            DC.JI_JAR_FULL_PATH,
            DC.JI_JAR_FILE_NAME,
            DC.JI_JAR_FILE_NAME_HEAD,
            DC.JI_JAR_FILE_NAME_EXT,
            DC.JI_LAST_MODIFIED_TIME,
            DC.JI_JAR_FILE_HASH,
            DC.JI_IMPORT_TIME,
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
            DC.LMI_LAMBDA_NEXT_IS_TERMINAL,
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
    }),
    DTIE_METHOD_ARG_ANNOTATION("method_arg_annotation", new String[]{
            DC.MAA_RECORD_ID,
            DC.MAA_METHOD_HASH,
            DC.MAA_ARG_SEQ,
            DC.MAA_ANNOTATION_NAME,
            DC.MAA_ATTRIBUTE_NAME,
            DC.MAA_ATTRIBUTE_TYPE,
            DC.MAA_ATTRIBUTE_VALUE,
            DC.MAA_FULL_METHOD,
            DC.MAA_SIMPLE_CLASS_NAME,
    }),
    DTIE_METHOD_ARG_GENERICS_TYPE("method_arg_generics_type", new String[]{
            DC.MAGT_RECORD_ID,
            DC.MAGT_METHOD_HASH,
            DC.MAGT_SIMPLE_CLASS_NAME,
            DC.MAGT_SEQ,
            DC.MAGT_TYPE,
            DC.MAGT_TYPE_SEQ,
            DC.MAGT_SIMPLE_GENERICS_TYPE,
            DC.MAGT_GENERICS_ARRAY_DIMENSIONS,
            DC.MAGT_TYPE_VARIABLES_NAME,
            DC.MAGT_WILDCARD,
            DC.MAGT_REFERENCE_TYPE,
            DC.MAGT_GENERICS_CATEGORY,
            DC.MAGT_GENERICS_TYPE,
            DC.MAGT_FULL_METHOD,
    }),
    DTIE_METHOD_ARGUMENT("method_argument", new String[]{
            DC.MARG_METHOD_HASH,
            DC.MARG_ARG_SEQ,
            DC.MARG_SIMPLE_ARG_TYPE,
            DC.MARG_ARG_NAME,
            DC.MARG_ARG_TYPE,
            DC.MARG_ARRAY_DIMENSIONS,
            DC.MARG_ARG_CATEGORY,
            DC.MARG_EXISTS_GENERICS_TYPE,
            DC.MARG_SIMPLE_CLASS_NAME,
            DC.MARG_FULL_METHOD,
    }),
    DTIE_METHOD_CALL("method_call", new String[]{
            DC.MC_CALL_ID,
            DC.MC_CALL_TYPE,
            DC.MC_CALLEE_OBJ_TYPE,
            DC.MC_ENABLED,
            DC.MC_CALLER_METHOD_HASH,
            DC.MC_CALLER_SIMPLE_CLASS_NAME,
            DC.MC_CALLER_METHOD_NAME,
            DC.MC_CALLER_FULL_METHOD,
            DC.MC_CALLER_LINE_NUMBER,
            DC.MC_CALLER_RETURN_TYPE,
            DC.MC_CALLEE_METHOD_HASH,
            DC.MC_CALLEE_SIMPLE_CLASS_NAME,
            DC.MC_CALLEE_METHOD_NAME,
            DC.MC_CALLEE_FULL_METHOD,
            DC.MC_CALL_FLAGS,
            DC.MC_RAW_RETURN_TYPE,
            DC.MC_ACTUAL_RETURN_TYPE,
            DC.MC_CALLER_JAR_NUM,
            DC.MC_CALLEE_JAR_NUM,
    }),
    DTIE_METHOD_CALL_INFO("method_call_info", new String[]{
            DC.MCI_RECORD_ID,
            DC.MCI_CALL_ID,
            DC.MCI_OBJ_ARGS_SEQ,
            DC.MCI_SEQ,
            DC.MCI_CALLER_METHOD_HASH,
            DC.MCI_TYPE,
            DC.MCI_ARRAY_FLAG,
            DC.MCI_VALUE_TYPE,
            DC.MCI_THE_VALUE,
    }),
    DTIE_METHOD_CALL_STATIC_FIELD("method_call_static_field", new String[]{
            DC.MCSF_RECORD_ID,
            DC.MCSF_CALL_ID,
            DC.MCSF_OBJ_ARGS_SEQ,
            DC.MCSF_SEQ,
            DC.MCSF_CALLER_METHOD_HASH,
            DC.MCSF_SIMPLE_CLASS_NAME,
            DC.MCSF_FIELD_NAME,
            DC.MCSF_SIMPLE_FIELD_TYPE,
            DC.MCSF_CLASS_NAME,
            DC.MCSF_FIELD_TYPE,
    }),
    DTIE_METHOD_INFO("method_info", new String[]{
            DC.MI_METHOD_HASH,
            DC.MI_SIMPLE_CLASS_NAME,
            DC.MI_ACCESS_FLAGS,
            DC.MI_METHOD_NAME,
            DC.MI_SIMPLE_RETURN_TYPE,
            DC.MI_RETURN_TYPE,
            DC.MI_RETURN_ARRAY_DIMENSIONS,
            DC.MI_RETURN_CATEGORY,
            DC.MI_RETURN_EXISTS_GENERICS_TYPE,
            DC.MI_CLASS_NAME,
            DC.MI_FULL_METHOD,
            DC.MI_METHOD_INSTRUCTIONS_HASH,
            DC.MI_JAR_NUM,
    }),
    DTIE_METHOD_LINE_NUMBER("method_line_number", new String[]{
            DC.MLN_METHOD_HASH,
            DC.MLN_SIMPLE_CLASS_NAME,
            DC.MLN_METHOD_NAME,
            DC.MLN_MIN_LINE_NUMBER,
            DC.MLN_MAX_LINE_NUMBER,
            DC.MLN_FULL_METHOD,
    }),
    DTIE_METHOD_RETURN_GENERICS_TYPE("method_return_generics_type", new String[]{
            DC.MRGT_RECORD_ID,
            DC.MRGT_METHOD_HASH,
            DC.MRGT_SIMPLE_CLASS_NAME,
            DC.MRGT_TYPE,
            DC.MRGT_TYPE_SEQ,
            DC.MRGT_SIMPLE_GENERICS_TYPE,
            DC.MRGT_GENERICS_ARRAY_DIMENSIONS,
            DC.MRGT_TYPE_VARIABLES_NAME,
            DC.MRGT_WILDCARD,
            DC.MRGT_REFERENCE_TYPE,
            DC.MRGT_GENERICS_CATEGORY,
            DC.MRGT_GENERICS_TYPE,
            DC.MRGT_FULL_METHOD,
    }),
    DTIE_MYBATIS_MS_TABLE("mybatis_ms_table", new String[]{
            DC.MMT_RECORD_ID,
            DC.MMT_MAPPER_SIMPLE_CLASS_NAME,
            DC.MMT_MAPPER_METHOD_NAME,
            DC.MMT_SQL_STATEMENT,
            DC.MMT_TABLE_SEQ,
            DC.MMT_TABLE_NAME,
            DC.MMT_MAPPER_CLASS_NAME,
            DC.MMT_XML_FILE_NAME,
            DC.MMT_XML_FILE_PATH,
    }),
    DTIE_MYBATIS_MS_WRITE_TABLE("mybatis_ms_write_table", new String[]{
            DC.MMWT_RECORD_ID,
            DC.MMWT_MAPPER_SIMPLE_CLASS_NAME,
            DC.MMWT_MAPPER_METHOD_NAME,
            DC.MMWT_SQL_STATEMENT,
            DC.MMWT_TABLE_NAME,
            DC.MMWT_MAPPER_CLASS_NAME,
            DC.MMWT_XML_FILE_NAME,
            DC.MMWT_XML_FILE_PATH,
    }),
    DTIE_SF_FIELD_METHOD_CALL("sf_field_method_call", new String[]{
            DC.SFFMC_RECORD_ID,
            DC.SFFMC_SIMPLE_CLASS_NAME,
            DC.SFFMC_FIELD_NAME,
            DC.SFFMC_SEQ,
            DC.SFFMC_CALL_ID,
            DC.SFFMC_FIELD_TYPE,
            DC.SFFMC_ARRAY_DIMENSIONS,
            DC.SFFMC_CLASS_NAME,
            DC.SFFMC_CALLEE_CLASS_NAME,
            DC.SFFMC_CALLEE_METHOD_NAME,
    }),
    DTIE_SPRING_BEAN("spring_bean", new String[]{
            DC.SPB_RECORD_ID,
            DC.SPB_SPRING_BEAN_NAME,
            DC.SPB_SEQ,
            DC.SPB_CLASS_NAME,
            DC.SPB_BEAN_TYPE,
    }),
    DTIE_SPRING_CONTROLLER("spring_controller", new String[]{
            DC.SPC_METHOD_HASH,
            DC.SPC_SEQ,
            DC.SPC_SHOW_URI,
            DC.SPC_CLASS_PATH,
            DC.SPC_METHOD_PATH,
            DC.SPC_ANNOTATION_ANNOTATION_NAME,
            DC.SPC_SIMPLE_CLASS_NAME,
            DC.SPC_MAYBE_FILE_UPLOAD,
            DC.SPC_MAYBE_FILE_DOWNLOAD,
            DC.SPC_FULL_METHOD,
    }),
    DTIE_SPRING_TASK("spring_task", new String[]{
            DC.SPT_RECORD_ID,
            DC.SPT_METHOD_HASH,
            DC.SPT_SPRING_BEAN_NAME,
            DC.SPT_CLASS_NAME,
            DC.SPT_METHOD_NAME,
            DC.SPT_TYPE,
            DC.SPT_FULL_METHOD,
    }),
    DTIE_FIELD_ANNOTATION("field_annotation", new String[]{
            DC.FA_RECORD_ID,
            DC.FA_SIMPLE_CLASS_NAME,
            DC.FA_FIELD_NAME,
            DC.FA_ANNOTATION_NAME,
            DC.FA_ATTRIBUTE_NAME,
            DC.FA_ATTRIBUTE_TYPE,
            DC.FA_ATTRIBUTE_VALUE,
            DC.FA_CLASS_NAME,
    }),
    DTIE_FIELD_INFO("field_info", new String[]{
            DC.FI_RECORD_ID,
            DC.FI_SIMPLE_CLASS_NAME,
            DC.FI_FIELD_NAME,
            DC.FI_FIELD_TYPE,
            DC.FI_ARRAY_DIMENSIONS,
            DC.FI_FIELD_CATEGORY,
            DC.FI_MODIFIERS,
            DC.FI_PRIMITIVE_TYPE,
            DC.FI_STATIC_FLAG,
            DC.FI_FINAL_FLAG,
            DC.FI_EXISTS_GET_METHOD,
            DC.FI_EXISTS_SET_METHOD,
            DC.FI_EXISTS_GENERICS_TYPE,
            DC.FI_CLASS_NAME,
    }),
    DTIE_GET_METHOD("get_method", DC.GET_SET_METHOD_COLUMNS),
    DTIE_SET_METHOD("set_method", DC.GET_SET_METHOD_COLUMNS),
    DTIE_SET_METHOD_ASSIGN_INFO("set_method_assign_info", new String[]{
            DC.SMAI_SET_RECORD_ID,
            DC.SMAI_SET_METHOD_CALL_ID,
            DC.SMAI_SEQ,
            DC.SMAI_STEP,
            DC.SMAI_FLD_RELATIONSHIP_ID,
            DC.SMAI_CURR_CALL_ID,
            DC.SMAI_CALLER_METHOD_HASH,
            DC.SMAI_CALLER_FULL_METHOD,
            DC.SMAI_CALLER_LINE_NUMBER,
            DC.SMAI_CALLEE_FULL_METHOD,
            DC.SMAI_SET_METHOD_HASH,
            DC.SMAI_SET_FULL_METHOD,
            DC.SMAI_SET_METHOD_IN_SUPER,
            DC.SMAI_FLAG,
            DC.SMAI_FLAG_DESC,
            DC.SMAI_ASSIGN_INFO,
            DC.SMAI_EQUIVALENT_CONVERSION,
    }),
    DTIE_FIELD_RELATIONSHIP("field_relationship", new String[]{
            DC.FR_FLD_RELATIONSHIP_ID,
            DC.FR_GET_METHOD_CALL_ID,
            DC.FR_SET_METHOD_CALL_ID,
            DC.FR_CALLER_FULL_METHOD,
            DC.FR_CALLER_LINE_NUMBER,
            DC.FR_GET_SIMPLE_CLASS_NAME,
            DC.FR_GET_METHOD_NAME,
            DC.FR_GET_CLASS_NAME,
            DC.FR_SET_SIMPLE_CLASS_NAME,
            DC.FR_SET_METHOD_NAME,
            DC.FR_SET_CLASS_NAME,
            DC.FR_VALID,
            DC.FR_TYPE,
            DC.FR_RELATIONSHIP_FLAGS,
            DC.FR_BEAN_UTIL_CALL_ID,
            DC.FR_BEAN_UTIL_METHOD,
    }),
    DTIE_MYBATIS_MS_COLUMN("mybatis_ms_column", new String[]{
            DC.MMC_RECORD_ID,
            DC.MMC_ENTITY_SIMPLE_CLASS_NAME,
            DC.MMC_ENTITY_FIELD_NAME,
            DC.MMC_COLUMN_NAME,
            DC.MMC_ENTITY_CLASS_NAME,
            DC.MMC_XML_FILE_NAME,
            DC.MMC_XML_FILE_PATH,
    }),
    DTIE_MYBATIS_MS_ENTITY("mybatis_ms_entity", new String[]{
            DC.MME_RECORD_ID,
            DC.MME_MAPPER_SIMPLE_CLASS_NAME,
            DC.MME_ENTITY_SIMPLE_CLASS_NAME,
            DC.MME_TABLE_NAME,
            DC.MME_MAPPER_CLASS_NAME,
            DC.MME_ENTITY_CLASS_NAME,
            DC.MME_XML_FILE_NAME,
            DC.MME_XML_FILE_PATH,
    }),
    DTIE_FIELD_GENERICS_TYPE("field_generics_type", new String[]{
            DC.FGT_RECORD_ID,
            DC.FGT_SIMPLE_CLASS_NAME,
            DC.FGT_FIELD_NAME,
            DC.FGT_TYPE,
            DC.FGT_TYPE_SEQ,
            DC.FGT_SIMPLE_GENERICS_TYPE,
            DC.FGT_GENERICS_ARRAY_DIMENSIONS,
            DC.FGT_TYPE_VARIABLES_NAME,
            DC.FGT_WILDCARD,
            DC.FGT_REFERENCE_TYPE,
            DC.FGT_GENERICS_CATEGORY,
            DC.FGT_GENERICS_TYPE,
            DC.FGT_CLASS_NAME,
    }),
    DTIE_PROPERTIES_CONF("properties_conf", new String[]{
            DC.PC_RECORD_ID,
            DC.PC_PROPERTIES_KEY,
            DC.PC_PROPERTIES_FILE_PATH,
            DC.PC_PROPERTIES_FILE_NAME,
            DC.PC_PROPERTIES_VALUE,
    }),
    DTIE_MYBATIS_MS_SET_COLUMN("mybatis_ms_set_column", new String[]{
            DC.MMSETC_RECORD_ID,
            DC.MMSETC_MAPPER_SIMPLE_CLASS_NAME,
            DC.MMSETC_MAPPER_METHOD_NAME,
            DC.MMSETC_TABLE_NAME,
            DC.MMSETC_COLUMN_NAME,
            DC.MMSETC_PARAM_OBJ_NAME,
            DC.MMSETC_PARAM_NAME,
            DC.MMSETC_PARAM_RAW_NAME,
            DC.MMSETC_MAPPER_CLASS_NAME,
            DC.MMSETC_XML_FILE_NAME,
            DC.MMSETC_XML_FILE_PATH,
    }),
    DTIE_MYBATIS_MS_WHERE_COLUMN("mybatis_ms_where_column", new String[]{
            DC.MMWC_RECORD_ID,
            DC.MMWC_MAPPER_SIMPLE_CLASS_NAME,
            DC.MMWC_MAPPER_METHOD_NAME,
            DC.MMWC_TABLE_NAME,
            DC.MMWC_COLUMN_NAME,
            DC.MMWC_OPERATION,
            DC.MMWC_PARAM_OBJ_NAME,
            DC.MMWC_PARAM_NAME,
            DC.MMWC_PARAM_RAW_NAME,
            DC.MMWC_PARAM_TYPE,
            DC.MMWC_MAPPER_CLASS_NAME,
            DC.MMWC_XML_FILE_NAME,
            DC.MMWC_XML_FILE_PATH,
    }),
    DTIE_MYBATIS_MS_SELECT_COLUMN("mybatis_ms_select_column", new String[]{
            DC.MMSELC_RECORD_ID,
            DC.MMSELC_MAPPER_SIMPLE_CLASS_NAME,
            DC.MMSELC_MAPPER_METHOD_NAME,
            DC.MMSELC_TABLE_NAME,
            DC.MMSELC_COLUMN_NAME,
            DC.MMSELC_COLUMN_ALIAS,
            DC.MMSELC_MAPPER_CLASS_NAME,
            DC.MMSELC_XML_FILE_NAME,
            DC.MMSELC_XML_FILE_PATH,
    }),
    DTIE_METHOD_RETURN_ARG_SEQ("method_return_arg_seq", new String[]{
            DC.MRAS_CALLER_METHOD_HASH,
            DC.MRAS_RETURN_ARG_SEQ,
            DC.MRAS_CALLER_FULL_METHOD,
            DC.MRAS_EQUIVALENT_CONVERSION,
    }),
    DTIE_METHOD_RETURN_CALL_ID("method_return_call_id", new String[]{
            DC.MRCI_CALLER_METHOD_HASH,
            DC.MRCI_RETURN_CALL_ID,
            DC.MRCI_CALLER_FULL_METHOD,
            DC.MRCI_EQUIVALENT_CONVERSION,
    }),
    DTIE_METHOD_CATCH("method_catch", new String[]{
            DC.MCTH_RECORD_ID,
            DC.MCTH_METHOD_HASH,
            DC.MCTH_SIMPLE_CLASS_NAME,
            DC.MCTH_METHOD_NAME,
            DC.MCTH_SIMPLE_CATCH_EXCEPTION_TYPE,
            DC.MCTH_CATCH_EXCEPTION_TYPE,
            DC.MCTH_CATCH_FLAG,
            DC.MCTH_TRY_START_LINE_NUMBER,
            DC.MCTH_TRY_END_LINE_NUMBER,
            DC.MCTH_TRY_MIN_CALL_ID,
            DC.MCTH_TRY_MAX_CALL_ID,
            DC.MCTH_CATCH_START_OFFSET,
            DC.MCTH_CATCH_END_OFFSET,
            DC.MCTH_CATCH_START_LINE_NUMBER,
            DC.MCTH_CATCH_END_LINE_NUMBER,
            DC.MCTH_CATCH_MIN_CALL_ID,
            DC.MCTH_CATCH_MAX_CALL_ID,
            DC.MCTH_FULL_METHOD,
    }),
    DTIE_METHOD_FINALLY("method_finally", new String[]{
            DC.MF_RECORD_ID,
            DC.MF_METHOD_HASH,
            DC.MF_SIMPLE_CLASS_NAME,
            DC.MF_TRY_CATCH,
            DC.MF_TRY_CATCH_START_LINE_NUMBER,
            DC.MF_TRY_CATCH_END_LINE_NUMBER,
            DC.MF_TRY_CATCH_MIN_CALL_ID,
            DC.MF_TRY_CATCH_MAX_CALL_ID,
            DC.MF_FINALLY_START_LINE_NUMBER,
            DC.MF_FULL_METHOD,
    }),
    DTIE_METHOD_THROW("method_throw", new String[]{
            DC.MT_RECORD_ID,
            DC.MT_METHOD_HASH,
            DC.MT_SIMPLE_CLASS_NAME,
            DC.MT_THROW_OFFSET,
            DC.MT_LINE_NUMBER,
            DC.MT_SEQ,
            DC.MT_THROW_EXCEPTION_TYPE,
            DC.MT_THROW_FLAG,
            DC.MT_CATCH_START_OFFSET,
            DC.MT_CATCH_EXCEPTION_VARIABLE_NAME,
            DC.MT_CALL_ID,
            DC.MT_FULL_METHOD,
    }),
    DTIE_MYBATIS_MS_GET_SET_DB("mybatis_ms_get_set_db", new String[]{
            DC.MMGSD_RECORD_ID,
            DC.MMGSD_FLD_RELATIONSHIP_ID,
            DC.MMGSD_GET_OR_SET,
            DC.MMGSD_GET_METHOD_CALL_ID,
            DC.MMGSD_SET_METHOD_CALL_ID,
            DC.MMGSD_DB_OPERATE,
            DC.MMGSD_TABLE_NAME,
            DC.MMGSD_COLUMN_NAME,
            DC.MMGSD_COLUMN_RELATE_DESC,
    }),
    DTIE_METHOD_CALL_METHOD_CALL_RETURN("method_call_method_call_return", new String[]{
            DC.MCMCR_RECORD_ID,
            DC.MCMCR_CALL_ID,
            DC.MCMCR_OBJ_ARGS_SEQ,
            DC.MCMCR_SEQ,
            DC.MCMCR_ARRAY_FLAG,
            DC.MCMCR_USE_RETURN_CALL_ID,
            DC.MCMCR_CALLEE_METHOD_HASH,
            DC.MCMCR_CALLEE_SIMPLE_CLASS_NAME,
            DC.MCMCR_CALLEE_METHOD_NAME,
            DC.MCMCR_CALLEE_FULL_METHOD,
    });

    private final String tableNameKeyword;

    private final String[] columns;

    DbTableInfoEnum(String tableNameKeyword, String[] columns) {
        this.tableNameKeyword = tableNameKeyword;
        this.columns = columns;
    }

    /**
     * 获取insert sql语句缓存key
     *
     * @return
     */
    public String getInsertSqlKey() {
        return "insert_" + ordinal();
    }

    /**
     * 获取数据库表名关键字
     *
     * @return
     */
    public String getTableNameKeyword() {
        return tableNameKeyword;
    }

    /**
     * 获取表名，固定使用前缀"jacg_"，在表名后拼接"_"、用于替换为appName的标志
     *
     * @return
     */
    public String getTableName() {
        return getTableName(JACGConstants.REPLACE_SQL_FLAG_APP_NAME);
    }

    /**
     * 获取表名，固定使用前缀"jacg_"，在表名后拼接"_"、appName、表名后缀
     *
     * @param appName     数据库表名后缀
     * @param tableSuffix 数据库表名后缀
     * @return
     */
    public String getTableName(String appName, String tableSuffix) {
        return getTableName(appName + tableSuffix);
    }

    private String getTableName(String flag) {
        return JACGConstants.TABLE_PREFIX + tableNameKeyword + JACGConstants.FLAG_UNDER_LINE + flag;
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
