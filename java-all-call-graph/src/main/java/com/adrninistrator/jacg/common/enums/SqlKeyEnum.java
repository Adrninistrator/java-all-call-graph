package com.adrninistrator.jacg.common.enums;

/**
 * @author adrninistrator
 * @date 2022/12/7
 * @description: 用于缓存sql语句的key
 */
public enum SqlKeyEnum {
    CN_QUERY_DUPLICATE_CLASS_BEFORE_UPDATE,
    CN_QUERY_SIMPLE_CLASS,
    CN_QUERY_CLASS,
    CN_QUERY_CLASS_LIKE,
    CN_QUERY_CLASS_EQUALS,
    CN_UPDATE_SIMPLE_2_FULL,
    MC_QUERY_CALLER_FULL_METHOD,
    MC_QUERY_TOP_METHOD,
    MC_QUERY_ONE_CALLEE,
    MC_QUERY_ONE_CALLEE_CHECK_LINE_NUM,
    MC_QUERY_CALLEE_ALL_METHODS,
    MC_QUERY_CALLER_ALL_METHODS,
    MC_QUERY_ONE_CALLER1,
    MC_QUERY_ONE_CALLER2,
    MC_QUERY_NOTICE_INFO,
    MC_QUERY_ALL_CALLER,
    MC_QUERY_CALLEE_SEQ_IN_CALLER,
    MC_QUERY_CALLER_FULL_METHOD_BY_HASH,
    MC_QUERY_CALLEE_FULL_METHOD_BY_HASH,
    MC_QUERY_CALLEE_FULL_METHOD_BY_CLASS_METHOD,
    MC_QUERY_CALLEE_FULL_METHOD_BY_ID,
    MC_QUERY_METHOD_CALL_BY_CALLEE_HASH,
    MC_QUERY_MAX_CALL_ID,
    MC_QUERY_CHECK_NORMAL_MC_BY_EE_HASH,
    MC_QUERY_FLAG_4EE,
    MC_QUERY_FLAG_4ER,
    MC_QUERY_FLAG_BY_ID,
    MC_QUERY_NORMAL_MC_BY_EECM,
    MC_QUERY_NORMAL_MC_BY_EECM_ALL_COLUMNS,
    MC_QUERY_CALLER_INFO_BY_CALLEE,
    MC_QUERY_CALL_ID_BY_CALLEE_CLASS_METHOD,
    MC_QUERY_CALL_ID_BY_CALLEE_CLASS_METHOD_LAST,
    MC_QUERY_BY_CALL_ID,
    MC_QUERY_MIN_CALL_ID_CALLEE_CLASS,
    MC_QUERY_ALL_BY_CALLEE_CLASS_METHODS,
    MC_QUERY_ALL_BY_CALLEE_CLASS_METHODS_LAST,
    MC_QUERY_ALL_BY_CALLEE_CLASS,
    MC_QUERY_ALL_BY_CALLEE_CLASS_LAST,
    MC_QUERY_CALLEE_METHODS_LIKE,
    MC_QUERY_MIN_CALL_ID_CALLER_METHOD,
    MC_QUERY_ALL_BY_CALLER_METHOD,
    MC_QUERY_ALL_BY_CALLER_METHOD_LAST,
    MC_QUERY_BY_CALLER_CLASS_LINE_NUMBER,
    MC_UPDATE_ENABLED,
    MC_UPDATE_FLAGS,
    MI_QUERY_METHOD_HASH,
    MI_QUERY_FLAGS,
    MI_QUERY_FULL_METHOD_BY_CLASS_METHOD,
    MI_QUERY_FULL_METHOD_BY_RETURN_TYPE,
    MI_QUERY_FULL_METHOD_BY_CN_PREFIX,
    MI_QUERY_FULL_METHOD_BY_CLASS,
    MI_QUERY_ALL_BY_CLASS_METHOD,
    MI_QUERY_ALL_BY_CLASS,
    MI_QUERY_ALL_BY_FULL_METHOD,
    MI_QUERY_ALL_BY_CLASS_JAR_METHOD_NEW,
    MI_QUERY_INSTRUCTIONS_HASH_BY_HASH_OLD,
    MLN_QUERY_METHOD_HASH,
    MLN_QUERY_METHOD,
    MRGT_QUERY_BY_METHOD_HASH,
    MRGT_QUERY_GT_BY_METHOD_HASH,
    MRGT_QUERY_BY_RETURN_TYPE,
    JI_QUERY_JAR_INFO,
    JI_QUERY_JAR_INFO_OLD,
    JI_QUERY_JAR_INFO_NEW,
    JI_QUERY_OUTPUT_DIR_PATH,
    MA_QUERY_FMAH_WITH_ANNOTATIONS,
    MA_QUERY_ANNOTATION_BY_METHOD_HASH,
    MA_QUERY_ALL_ATTRIBUTES,
    MA_QUERY_SINGLE_ATTRIBUTE_BY_METHOD_HASH,
    MAA_QUERY_SINGLE_ATTRIBUTE_BY_METHOD_HASH,
    CA_QUERY_SIMPLE_CLASS_NAME_WITH_ANNOTATION,
    CA_QUERY_CLASS_NAME_WITH_ANNOTATION,
    CA_QUERY_ANNOTATIONS_BY_SIMPLE_CLASS_NAME,
    CA_QUERY_ONE_ANNOTATION_BY_SIMPLE_CLASS_NAME,
    CA_QUERY_ONE_ANNOTATION_BY_ANNOTATION_ATTRIBUTE,
    CI_QUERY_ACCESS_FLAGS,
    CI_QUERY_ALL_BY_ID,
    CI_QUERY_ALL_BY_ID_LAST,
    CI_QUERY_BY_PACKAGE_PREFIX,
    CI_QUERY_ALL_BY_JAR_NUM_NEW,
    CI_QUERY_HASH_BY_CLASS_JAR_OLD,
    CEIGT_QUERY_SUPER_INTERFACE_CLASS_NAME,
    CEIGT_QUERY_GENERICS_TYPE,
    BD_QUERY_BUSINESS_DATA,
    BD_QUERY_METHOD_BY_BUSINESS_DATA,
    BD_DELETE_BY_TYPE,
    EI_QUERY_DOWNWARD,
    EI_QUERY_UPWARD_EXTENDS,
    EI_QUERY_UPWARD_IMPL,
    EI_QUERY_UPWARD,
    MCI_QUERY_ALL_BY_CALL_ID,
    MCI_QUERY_ALL_BY_CALL_ID_OBJ_ARGS_SEQ,
    MCI_QUERY_ALL_BY_ID_TYPE_ARG,
    MCI_QUERY_ALL_BY_ID_TYPE_ARG_LAST,
    MCI_QUERY_4_CALLER_BY_TYPE_VALUE,
    MCI_QUERY_BY_MC_ID_TYPE,
    MCI_QUERY_METHOD_ARG_USAGE,
    LMI_QUERY_CALLEE_INFO,
    MMT_QUERY_TABLE_BY_MAPPER,
    MMT_QUERY_BY_MAPPER,
    MMWT_QUERY_TABLE,
    ACP_QUERY,
    MAGT_QUERY,
    MAGT_QUERY_BY_ARG_TYPE,
    MAGT_QUERY_BY_METHOD_ARG,
    MARG_QUERY_BY_ARG_TYPE,
    MARG_QUERY_NAME,
    MARG_QUERY_BY_HASH,
    SPC_QUERY_ALL,
    SPC_QUERY_ALL_SCN,
    SPC_QUERY_BY_METHOD,
    SPC_QUERY_BY_SCN,
    SPC_QUERY_FILE_UPLOAD,
    SPC_QUERY_FILE_DOWNLOAD,
    SPT_QUERY,
    SPT_QUERY_BY_FULL_METHOD,
    FA_QUERY_CLASS_FIELD_WITH_ANNOTATIONS,
    FA_QUERY_ONE_ANNOTATION_BY_CLASS_FIELD,
    GM_QUERY_BY_CLASS_NAME,
    GM_QUERY_BY_METHOD_NAME,
    GM_QUERY_BY_FIELD_NAME,
    GM_QUERY_BY_SET_METHOD,
    GM_QUERY_BY_FIELD_TYPE,
    GM_FGT_QUERY_UNIQUE_CUSTOM_FIELD,
    GM_QUERY_UPPER_NESTED_FIELD_TYPE,
    GM_FGT_QUERY_UPPER_NESTED_FIELD_TYPE,
    SM_QUERY_BY_CLASS_NAME,
    SM_QUERY_BY_METHOD_NAME,
    SM_QUERY_BY_FIELD_NAME,
    SM_QUERY_BY_GET_METHOD,
    SM_QUERY_BY_FIELD_TYPE,
    FR_QUERY_SET_METHOD_4GET,
    FR_QUERY_GET_METHOD_4SET,
    FR_QUERY_MAX_RECORD_ID,
    FR_QUERY_CALLER_METHOD,
    FR_QUERY_ALL_BY_ID,
    FR_QUERY_ALL_BY_ID_LAST,
    FR_QUERY_BY_GET_METHOD_CALL_ID,
    FR_QUERY_BY_SET_METHOD_CALL_ID,
    FR_UPDATE_FLAGS,
    MME_QUERY_TABLE_NAME_BY_ENTITY,
    MME_QUERY_BY_ENTITY,
    MME_QUERY_ALL_ENTITY,
    MME_QUERY_MULTI_ENTITY,
    MME_QUERY_BY_ENTITY_SIMPLE_CLASS_NAME,
    MME_QUERY_ALL_BY_ID,
    MME_QUERY_ALL_BY_ID_LAST,
    MMC_QUERY_TABLE_COLUMN_NAME,
    FGT_QUERY_BY_CLASS_FIELD_NAME,
    FGT_QUERY_BY_FIELD_GENERICS_TYPE,
    FI_QUERY_BY_CLASS_PACKAGE_EXCLUDE_PSF,
    FI_QUERY_BY_CLASS_CUSTOM_TYPE,
    FI_QUERY_BY_CLASS_NAME,
    FI_QUERY_4DTO_BY_CLASS_NAME,
    FI_QUERY_ALL_SIMPLE_CLASS_NAME,
    PC_QUERY_VALUE_BY_KEY,
    PC_QUERY_ALL_BY_KEY,
    PC_QUERY_ALL_BY_KEY_LIKE,
    PC_QUERY_ALL_BY_FILE,
    PC_QUERY_MAX_RECORD_ID,
    MMWC_QUERY_COLUMN_INFO_BY_MAPPER,
    MMSETC_QUERY_ALL_BY_PARAMETER,
    MMSETC_QUERY_COLUMN_INFO_BY_MAPPER,
    MMMAD_QUERY_BY_FLD_RELATIONSHIP_ID,
    MMMAD_QUERY_BY_GET_METHOD_CALL_ID,
    MMMAD_QUERY_BY_SET_METHOD_CALL_ID,
    MMSELC_QUERY_COLUMN_INFO_BY_MAPPER,
    MRCI_QUERY,
    MRAS_QUERY,
    MCTH_QUERY_BY_TYPE,
    MCTH_QUERY_SIMPLE_CLASS_NAME,
    MCTH_QUERY_BY_SIMPLE_CLASS_NAME,
    MT_QUERY_BY_METHOD_HASH_CATCH_START_OFFSET,
    MT_QUERY_BY_METHOD_HASH_THROW_OFFSET_RANGE,
    MT_QUERY_BY_METHOD_HASH_CALL_ID,
    MCSF_QUERY_CLASS_ALL_FIELD_NAME,
    MCSF_QUERY_BY_CLASS_FIELD,
    JAVACG2C_QUERY_VALUE,
}
