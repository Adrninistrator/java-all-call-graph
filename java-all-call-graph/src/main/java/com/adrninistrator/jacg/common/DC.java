package com.adrninistrator.jacg.common;

/**
 * @author adrninistrator
 * @date 2021/6/18
 * @description: 数据库表列名
 */

public class DC {
    public static final String COMMON_ANNOTATION_ATTRIBUTE_NAME = "attribute_name";
    public static final String COMMON_ANNOTATION_ATTRIBUTE_TYPE = "attribute_type";
    public static final String COMMON_ANNOTATION_ATTRIBUTE_VALUE = "attribute_value";

    public static final String COMMON_TABLE_NAME = "table_name";
    public static final String COMMON_COLUMN_NAME = "column_name";
    public static final String COMMON_PARAM_OBJ_NAME = "param_obj_name";
    public static final String COMMON_PARAM_NAME = "param_name";
    public static final String COMMON_PARAM_RAW_NAME = "param_raw_name";

    public static final String JAVACG2C_CONFIG_FILE_NAME = "config_file_name";
    public static final String JAVACG2C_CONFIG_KEY = "config_key";
    public static final String JAVACG2C_CONFIG_VALUE = "config_value";
    public static final String JAVACG2C_CONFIG_TYPE = "config_type";

    public static final String CN_RECORD_ID = "record_id";
    public static final String CN_CLASS_NAME = "class_name";
    public static final String CN_SIMPLE_CLASS_NAME = "simple_class_name";
    public static final String CN_DUPLICATE_CLASS = "duplicate_class";

    public static final String CR_RECORD_ID = "record_id";
    public static final String CR_CLASS_NAME = "class_name";
    public static final String CR_SIMPLE_CLASS_NAME = "simple_class_name";
    public static final String CR_REFERENCED_CLASS_NAME = "referenced_class_name";
    public static final String CR_REFERENCED_SIMPLE_CLASS_NAME = "referenced_simple_class_name";

    public static final String CA_RECORD_ID = "record_id";
    public static final String CA_SIMPLE_CLASS_NAME = "simple_class_name";
    public static final String CA_ANNOTATION_NAME = "annotation_name";
    public static final String CA_ATTRIBUTE_NAME = COMMON_ANNOTATION_ATTRIBUTE_NAME;
    public static final String CA_ATTRIBUTE_TYPE = COMMON_ANNOTATION_ATTRIBUTE_TYPE;
    public static final String CA_ATTRIBUTE_VALUE = COMMON_ANNOTATION_ATTRIBUTE_VALUE;
    public static final String CA_CLASS_NAME = "class_name";

    public static final String MA_RECORD_ID = "record_id";
    public static final String MA_METHOD_HASH = "method_hash";
    public static final String MA_ANNOTATION_NAME = "annotation_name";
    public static final String MA_ATTRIBUTE_NAME = COMMON_ANNOTATION_ATTRIBUTE_NAME;
    public static final String MA_ATTRIBUTE_TYPE = COMMON_ANNOTATION_ATTRIBUTE_TYPE;
    public static final String MA_ATTRIBUTE_VALUE = COMMON_ANNOTATION_ATTRIBUTE_VALUE;
    public static final String MA_FULL_METHOD = "full_method";
    public static final String MA_RETURN_TYPE = "return_type";
    public static final String MA_SIMPLE_CLASS_NAME = "simple_class_name";

    public static final String MAA_RECORD_ID = "record_id";
    public static final String MAA_METHOD_HASH = "method_hash";
    public static final String MAA_ARG_SEQ = "arg_seq";
    public static final String MAA_ANNOTATION_NAME = "annotation_name";
    public static final String MAA_ATTRIBUTE_NAME = COMMON_ANNOTATION_ATTRIBUTE_NAME;
    public static final String MAA_ATTRIBUTE_TYPE = COMMON_ANNOTATION_ATTRIBUTE_TYPE;
    public static final String MAA_ATTRIBUTE_VALUE = COMMON_ANNOTATION_ATTRIBUTE_VALUE;
    public static final String MAA_FULL_METHOD = "full_method";
    public static final String MAA_RETURN_TYPE = "return_type";
    public static final String MAA_SIMPLE_CLASS_NAME = "simple_class_name";

    public static final String MC_CALL_ID = "call_id";
    public static final String MC_ENABLED = "enabled";
    public static final String MC_CALL_TYPE = "call_type";
    public static final String MC_CALLER_METHOD_HASH = "caller_method_hash";
    public static final String MC_CALLER_FULL_METHOD = "caller_full_method";
    public static final String MC_CALLER_METHOD_NAME = "caller_method_name";
    public static final String MC_CALLER_SIMPLE_CLASS_NAME = "caller_simple_class_name";
    public static final String MC_CALLER_LINE_NUMBER = "caller_line_number";
    public static final String MC_CALLER_RETURN_TYPE = "caller_return_type";
    public static final String MC_CALLEE_METHOD_HASH = "callee_method_hash";
    public static final String MC_CALLEE_SIMPLE_CLASS_NAME = "callee_simple_class_name";
    public static final String MC_CALLEE_METHOD_NAME = "callee_method_name";
    public static final String MC_CALLEE_FULL_METHOD = "callee_full_method";
    public static final String MC_CALLEE_ARRAY_DIMENSIONS = "callee_array_dimensions";
    public static final String MC_CALLEE_OBJ_TYPE = "callee_obj_type";
    public static final String MC_RAW_RETURN_TYPE = "raw_return_type";
    public static final String MC_ACTUAL_RETURN_TYPE = "actual_return_type";
    public static final String MC_CALL_FLAGS = "call_flags";
    public static final String MC_CALLER_JAR_NUM = "caller_jar_num";
    public static final String MC_CALLEE_JAR_NUM = "callee_jar_num";
    public static final String MC_DESCRIPTION = "description";

    public static final String LMI_CALL_ID = "call_id";
    public static final String LMI_LAMBDA_CALLEE_CLASS_NAME = "lambda_callee_class_name";
    public static final String LMI_LAMBDA_CALLEE_METHOD_NAME = "lambda_callee_method_name";
    public static final String LMI_LAMBDA_CALLEE_FULL_METHOD = "lambda_callee_full_method";
    public static final String LMI_LAMBDA_NEXT_CLASS_NAME = "lambda_next_class_name";
    public static final String LMI_LAMBDA_NEXT_METHOD_NAME = "lambda_next_method_name";
    public static final String LMI_LAMBDA_NEXT_FULL_METHOD = "lambda_next_full_method";
    public static final String LMI_LAMBDA_NEXT_IS_STREAM = "lambda_next_is_stream";
    public static final String LMI_LAMBDA_NEXT_IS_INTERMEDIATE = "lambda_next_is_intermediate";
    public static final String LMI_LAMBDA_NEXT_IS_TERMINAL = "lambda_next_is_terminal";

    public static final String MLN_METHOD_HASH = "method_hash";
    public static final String MLN_RECORD_ID = "record_id";
    public static final String MLN_SIMPLE_CLASS_NAME = "simple_class_name";
    public static final String MLN_METHOD_NAME = "method_name";
    public static final String MLN_MIN_LINE_NUMBER = "min_line_number";
    public static final String MLN_MAX_LINE_NUMBER = "max_line_number";
    public static final String MLN_FULL_METHOD = "full_method";
    public static final String MLN_RETURN_TYPE = "return_type";

    public static final String JI_JAR_NUM = "jar_num";
    public static final String JI_JAR_TYPE = "jar_type";
    public static final String JI_JAR_PATH_HASH = "jar_path_hash";
    public static final String JI_JAR_FULL_PATH = "jar_full_path";
    public static final String JI_JAR_FILE_NAME = "jar_file_name";
    public static final String JI_JAR_FILE_NAME_HEAD = "jar_file_name_head";
    public static final String JI_JAR_FILE_NAME_EXT = "jar_file_name_ext";
    public static final String JI_LAST_MODIFIED_TIME = "last_modified_time";
    public static final String JI_JAR_FILE_HASH = "jar_file_hash";
    public static final String JI_INNER_JAR_PATH = "inner_jar_path";
    public static final String JI_INNER_JAR_FILE_NAME = "inner_jar_file_name";
    public static final String JI_IMPORT_TIME = "import_time";

    public static final String BD_CALL_ID = "call_id";
    public static final String BD_DATA_TYPE = "data_type";
    public static final String BD_DATA_VALUE = "data_value";

    public static final String PCD_RECORD_ID = "record_id";
    public static final String PCD_DATA_TYPE = "data_type";
    public static final String PCD_DATA_KEY = "data_key";
    public static final String PCD_DATA_VALUE = "data_value";

    public static final String CI_RECORD_ID = "record_id";
    public static final String CI_SIMPLE_CLASS_NAME = "simple_class_name";
    public static final String CI_ACCESS_FLAGS = "access_flags";
    public static final String CI_CLASS_NAME = "class_name";
    public static final String CI_PACKAGE_NAME = "package_name";
    public static final String CI_PACKAGE_LEVEL = "package_level";
    public static final String CI_CLASS_FILE_HASH = "class_file_hash";
    public static final String CI_JAR_NUM = "jar_num";
    public static final String CI_CLASS_PATH_IN_JAR = "class_path_in_jar";

    public static final String[] CLASS_INFO_COLUMNS = new String[]{
            CI_RECORD_ID,
            CI_SIMPLE_CLASS_NAME,
            CI_ACCESS_FLAGS,
            CI_CLASS_NAME,
            CI_PACKAGE_NAME,
            CI_PACKAGE_LEVEL,
            CI_CLASS_FILE_HASH,
            CI_JAR_NUM,
            CI_CLASS_PATH_IN_JAR
    };

    public static final String MI_RECORD_ID = "record_id";
    public static final String MI_METHOD_HASH = "method_hash";
    public static final String MI_SIMPLE_CLASS_NAME = "simple_class_name";
    public static final String MI_ACCESS_FLAGS = "access_flags";
    public static final String MI_METHOD_NAME = "method_name";
    public static final String MI_SIMPLE_RETURN_TYPE_NAD = "simple_return_type_nad";
    public static final String MI_RETURN_TYPE_NAD = "return_type_nad";
    public static final String MI_RETURN_ARRAY_DIMENSIONS = "return_array_dimensions";
    public static final String MI_RETURN_TYPE = "return_type";
    public static final String MI_RETURN_CATEGORY = "return_category";
    public static final String MI_RETURN_EXISTS_GENERICS_TYPE = "return_exists_generics_type";
    public static final String MI_METHOD_INSTRUCTIONS_HASH = "method_instructions_hash";
    public static final String MI_CLASS_NAME = "class_name";
    public static final String MI_FULL_METHOD = "full_method";
    public static final String MI_JAR_NUM = "jar_num";

    public static final String[] METHOD_INFO_COLUMNS = new String[]{
            MI_RECORD_ID,
            MI_METHOD_HASH,
            MI_SIMPLE_CLASS_NAME,
            MI_ACCESS_FLAGS,
            MI_METHOD_NAME,
            MI_SIMPLE_RETURN_TYPE_NAD,
            MI_RETURN_TYPE_NAD,
            MI_RETURN_ARRAY_DIMENSIONS,
            MI_RETURN_TYPE,
            MI_RETURN_CATEGORY,
            MI_RETURN_EXISTS_GENERICS_TYPE,
            MI_CLASS_NAME,
            MI_FULL_METHOD,
            MI_METHOD_INSTRUCTIONS_HASH,
            MI_JAR_NUM
    };

    public static final String EI_RECORD_ID = "record_id";
    public static final String EI_SIMPLE_CLASS_NAME = "simple_class_name";
    public static final String EI_CLASS_NAME = "class_name";
    public static final String EI_ACCESS_FLAGS = "access_flags";
    public static final String EI_TYPE = "type";
    public static final String EI_SEQ = "seq";
    public static final String EI_EXISTS_DOWNWARD_CLASSES = "exists_downward_classes";
    public static final String EI_UPWARD_SIMPLE_CLASS_NAME = "upward_simple_class_name";
    public static final String EI_UPWARD_CLASS_NAME = "upward_class_name";

    public static final String EIAF_RECORD_ID = "record_id";
    public static final String EIAF_SIMPLE_CLASS_NAME = "simple_class_name";
    public static final String EIAF_ARG_SEQ = "arg_seq";
    public static final String EIAF_FIELD_TYPE = "field_type";
    public static final String EIAF_FIELD_NAME = "field_name";
    public static final String EIAF_CLASS_NAME = "class_name";
    public static final String EIAF_FULL_METHOD = "full_method";

    public static final String EIAI_RECORD_ID = "record_id";
    public static final String EIAI_SIMPLE_CLASS_NAME = "simple_class_name";
    public static final String EIAI_CONST_NAME = "const_name";
    public static final String EIAI_ORDINAL = "ordinal";
    public static final String EIAI_ARG_SEQ = "arg_seq";
    public static final String EIAI_FIELD_TYPE = "field_type";
    public static final String EIAI_FIELD_VALUE = "field_value";
    public static final String EIAI_CLASS_NAME = "class_name";
    public static final String EIAI_FULL_METHOD = "full_method";

    public static final String MCI_RECORD_ID = "record_id";
    public static final String MCI_CALL_ID = "call_id";
    public static final String MCI_OBJ_ARGS_SEQ = "obj_args_seq";
    public static final String MCI_SEQ = "seq";
    public static final String MCI_CALLER_METHOD_HASH = "caller_method_hash";
    public static final String MCI_TYPE = "type";
    // H2数据库中array是关键字不能使用
    public static final String MCI_ARRAY_FLAG = "array_flag";
    public static final String MCI_VALUE_TYPE = "value_type";
    // H2中使用"value"作为字段名会报错
    public static final String MCI_THE_VALUE = "the_value";

    public static final String SPB_RECORD_ID = "record_id";
    public static final String SPB_SPRING_BEAN_NAME = "spring_bean_name";
    public static final String SPB_SEQ = "seq";
    public static final String SPB_SIMPLE_CLASS_NAME = "simple_class_name";
    public static final String SPB_CLASS_NAME = "class_name";
    public static final String SPB_BEAN_TYPE = "bean_type";

    public static final String SPC_RECORD_ID = "record_id";
    public static final String SPC_METHOD_HASH = "method_hash";
    public static final String SPC_SEQ = "seq";
    public static final String SPC_SHOW_URI = "show_uri";
    public static final String SPC_CLASS_PATH = "class_path";
    public static final String SPC_METHOD_PATH = "method_path";
    public static final String SPC_ANNOTATION_ANNOTATION_NAME = "annotation_name";
    public static final String SPC_SIMPLE_CLASS_NAME = "simple_class_name";
    public static final String SPC_MAYBE_FILE_UPLOAD = "maybe_file_upload";
    public static final String SPC_MAYBE_FILE_DOWNLOAD = "maybe_file_download";
    public static final String SPC_FULL_METHOD = "full_method";
    public static final String SPC_RETURN_TYPE = "return_type";

    public static final String SPT_RECORD_ID = "record_id";
    public static final String SPT_METHOD_HASH = "method_hash";
    public static final String SPT_SPRING_BEAN_NAME = "spring_bean_name";
    public static final String SPT_CLASS_NAME = "class_name";
    public static final String SPT_METHOD_NAME = "method_name";
    public static final String SPT_TYPE = "type";
    public static final String SPT_FULL_METHOD = "full_method";
    public static final String SPT_RETURN_TYPE = "return_type";

    public static final String CEIGT_RECORD_ID = "record_id";
    public static final String CEIGT_SIMPLE_CLASS_NAME = "simple_class_name";
    public static final String CEIGT_EXT_TYPE = "ext_type";
    public static final String CEIGT_SEQ = "seq";
    public static final String CEIGT_SUPER_ITF_SIMPLE_CLASS_NAME = "super_itf_simple_class_name";
    public static final String CEIGT_GENERICS_SEQ = "generics_seq";
    public static final String CEIGT_SIMPLE_GENERICS_TYPE_NAD = "simple_generics_type_nad";
    public static final String CEIGT_GENERICS_ARRAY_DIMENSIONS = "generics_array_dimensions";
    public static final String CEIGT_TYPE_VARIABLES_NAME = "type_variables_name";
    public static final String CEIGT_GENERICS_CATEGORY = "generics_category";
    public static final String CEIGT_GENERICS_TYPE_NAD = "generics_type_nad";
    public static final String CEIGT_CLASS_NAME = "class_name";
    public static final String CEIGT_SUPER_ITF_CLASS_NAME = "super_itf_class_name";

    public static final String CSGT_RECORD_ID = "record_id";
    public static final String CSGT_SIMPLE_CLASS_NAME = "simple_class_name";
    public static final String CSGT_SEQ = "seq";
    public static final String CSGT_TYPE_VARIABLES_NAME = "type_variables_name";
    public static final String CSGT_GENERICS_EXTENDS_CLASS_NAME = "generics_extends_class_name";
    public static final String CSGT_CLASS_NAME = "class_name";

    public static final String MMT_RECORD_ID = "record_id";
    public static final String MMT_MAPPER_SIMPLE_CLASS_NAME = "mapper_simple_class_name";
    public static final String MMT_MAPPER_METHOD_NAME = "mapper_method_name";
    public static final String MMT_SQL_STATEMENT = "sql_statement";
    public static final String MMT_TABLE_SEQ = "table_seq";
    public static final String MMT_TABLE_NAME = "table_name";
    public static final String MMT_MAPPER_CLASS_NAME = "mapper_class_name";
    public static final String MMT_XML_FILE_NAME = "xml_file_name";
    public static final String MMT_XML_FILE_PATH = "xml_file_path";

    public static final String MMWT_RECORD_ID = "record_id";
    public static final String MMWT_MAPPER_SIMPLE_CLASS_NAME = "mapper_simple_class_name";
    public static final String MMWT_MAPPER_METHOD_NAME = "mapper_method_name";
    public static final String MMWT_SQL_STATEMENT = "sql_statement";
    public static final String MMWT_TABLE_NAME = "table_name";
    public static final String MMWT_MAPPER_CLASS_NAME = "mapper_class_name";
    public static final String MMWT_XML_FILE_NAME = "xml_file_name";
    public static final String MMWT_XML_FILE_PATH = "xml_file_path";

    public static final String MAGT_RECORD_ID = "record_id";
    public static final String MAGT_METHOD_HASH = "method_hash";
    public static final String MAGT_SIMPLE_CLASS_NAME = "simple_class_name";
    public static final String MAGT_SEQ = "seq";
    public static final String MAGT_TYPE = "type";
    public static final String MAGT_TYPE_SEQ = "type_seq";
    public static final String MAGT_SIMPLE_GENERICS_TYPE_NAD = "simple_generics_type_nad";
    public static final String MAGT_GENERICS_ARRAY_DIMENSIONS = "generics_array_dimensions";
    public static final String MAGT_TYPE_VARIABLES_NAME = "type_variables_name";
    public static final String MAGT_WILDCARD = "wildcard";
    public static final String MAGT_REFERENCE_TYPE = "reference_type";
    public static final String MAGT_GENERICS_CATEGORY = "generics_category";
    public static final String MAGT_GENERICS_TYPE_NAD = "generics_type_nad";
    public static final String MAGT_FULL_METHOD = "full_method";
    public static final String MAGT_RETURN_TYPE = "return_type";

    public static final String MARG_RECORD_ID = "record_id";
    public static final String MARG_METHOD_HASH = "method_hash";
    public static final String MARG_ARG_SEQ = "arg_seq";
    public static final String MARG_SIMPLE_ARG_TYPE_NAD = "simple_arg_type_nad";
    public static final String MARG_ARG_NAME = "arg_name";
    public static final String MARG_ARG_TYPE_NAD = "arg_type_nad";
    public static final String MARG_ARRAY_DIMENSIONS = "array_dimensions";
    public static final String MARG_ARG_CATEGORY = "arg_category";
    public static final String MARG_EXISTS_GENERICS_TYPE = "exists_generics_type";
    public static final String MARG_SIMPLE_CLASS_NAME = "simple_class_name";
    public static final String MARG_FULL_METHOD = "full_method";
    public static final String MARG_RETURN_TYPE = "return_type";

    public static final String MRGT_RECORD_ID = "record_id";
    public static final String MRGT_METHOD_HASH = "method_hash";
    public static final String MRGT_SIMPLE_CLASS_NAME = "simple_class_name";
    public static final String MRGT_TYPE = "type";
    public static final String MRGT_TYPE_SEQ = "type_seq";
    public static final String MRGT_SIMPLE_GENERICS_TYPE_NAD = "simple_generics_type_nad";
    public static final String MRGT_GENERICS_ARRAY_DIMENSIONS = "generics_array_dimensions";
    public static final String MRGT_TYPE_VARIABLES_NAME = "type_variables_name";
    public static final String MRGT_WILDCARD = "wildcard";
    public static final String MRGT_REFERENCE_TYPE = "reference_type";
    public static final String MRGT_GENERICS_CATEGORY = "generics_category";
    public static final String MRGT_GENERICS_TYPE_NAD = "generics_type_nad";
    public static final String MRGT_FULL_METHOD = "full_method";
    public static final String MRGT_RETURN_TYPE = "return_type";

    public static final String IC_INNER_SIMPLE_CLASS_NAME = "inner_simple_class_name";
    public static final String IC_INNER_CLASS_NAME = "inner_class_name";
    public static final String IC_OUTER_SIMPLE_CLASS_NAME = "outer_simple_class_name";
    public static final String IC_OUTER_CLASS_NAME = "outer_class_name";
    public static final String IC_ANONYMOUS_CLASS = "anonymous_class";

    public static final String FA_RECORD_ID = "record_id";
    public static final String FA_SIMPLE_CLASS_NAME = "simple_class_name";
    public static final String FA_FIELD_NAME = "field_name";
    public static final String FA_ANNOTATION_NAME = "annotation_name";
    public static final String FA_ATTRIBUTE_NAME = COMMON_ANNOTATION_ATTRIBUTE_NAME;
    public static final String FA_ATTRIBUTE_TYPE = COMMON_ANNOTATION_ATTRIBUTE_TYPE;
    public static final String FA_ATTRIBUTE_VALUE = COMMON_ANNOTATION_ATTRIBUTE_VALUE;
    public static final String FA_CLASS_NAME = "class_name";

    public static final String FI_RECORD_ID = "record_id";
    public static final String FI_SIMPLE_CLASS_NAME = "simple_class_name";
    public static final String FI_FIELD_NAME = "field_name";
    public static final String FI_FIELD_TYPE_NAD = "field_type_nad";
    public static final String FI_ARRAY_DIMENSIONS = "array_dimensions";
    public static final String FI_FIELD_CATEGORY = "field_category";
    public static final String FI_MODIFIERS = "modifiers";
    public static final String FI_PRIMITIVE_TYPE = "primitive_type";
    public static final String FI_STATIC_FLAG = "static_flag";
    public static final String FI_FINAL_FLAG = "final_flag";
    public static final String FI_EXISTS_GET_METHOD = "exists_get_method";
    public static final String FI_EXISTS_SET_METHOD = "exists_set_method";
    public static final String FI_EXISTS_GENERICS_TYPE = "exists_generics_type";
    public static final String FI_CLASS_NAME = "class_name";

    public static final String GSM_RECORD_ID = "record_id";
    public static final String GSM_SIMPLE_CLASS_NAME = "simple_class_name";
    public static final String GSM_METHOD_NAME = "method_name";
    public static final String GSM_FIELD_NAME = "field_name";
    public static final String GSM_FIELD_CATEGORY = "field_category";
    public static final String GSM_SIMPLE_FIELD_TYPE_NAD = "simple_field_type_nad";
    public static final String GSM_FIELD_TYPE_NAD = "field_type_nad";
    public static final String GSM_ARRAY_DIMENSIONS = "array_dimensions";
    public static final String GSM_CLASS_NAME = "class_name";
    public static final String GSM_METHOD_HASH = "method_hash";
    public static final String GSM_FULL_METHOD = "full_method";
    public static final String GSM_RETURN_TYPE = "return_type";

    // get/set方法信息表的字段
    public static final String[] GET_SET_METHOD_COLUMNS = new String[]{
            GSM_RECORD_ID,
            GSM_SIMPLE_CLASS_NAME,
            GSM_METHOD_NAME,
            GSM_FIELD_NAME,
            GSM_FIELD_CATEGORY,
            GSM_SIMPLE_FIELD_TYPE_NAD,
            GSM_FIELD_TYPE_NAD,
            GSM_ARRAY_DIMENSIONS,
            GSM_CLASS_NAME,
            GSM_METHOD_HASH,
            GSM_FULL_METHOD,
            GSM_RETURN_TYPE
    };

    public static final String FR_FLD_RELATIONSHIP_ID = "fld_relationship_id";
    public static final String FR_GET_METHOD_CALL_ID = "get_method_call_id";
    public static final String FR_SET_METHOD_CALL_ID = "set_method_call_id";
    public static final String FR_CALLER_FULL_METHOD = "caller_full_method";
    public static final String FR_CALLER_LINE_NUMBER = "caller_line_number";
    public static final String FR_SET_SIMPLE_CLASS_NAME = "set_simple_class_name";
    public static final String FR_SET_METHOD_NAME = "set_method_name";
    public static final String FR_SET_CLASS_NAME = "set_class_name";
    public static final String FR_GET_SIMPLE_CLASS_NAME = "get_simple_class_name";
    public static final String FR_GET_METHOD_NAME = "get_method_name";
    public static final String FR_GET_CLASS_NAME = "get_class_name";
    public static final String FR_VALID = "valid";
    public static final String FR_TYPE = "type";
    public static final String FR_RELATIONSHIP_FLAGS = "relationship_flags";
    public static final String FR_BEAN_UTIL_CALL_ID = "bean_util_call_id";
    public static final String FR_BEAN_UTIL_METHOD = "bean_util_method";

    public static final String MMC_RECORD_ID = "record_id";
    public static final String MMC_ENTITY_SIMPLE_CLASS_NAME = "entity_simple_class_name";
    public static final String MMC_ENTITY_FIELD_NAME = "entity_field_name";
    public static final String MMC_COLUMN_NAME = "column_name";
    public static final String MMC_ENTITY_CLASS_NAME = "entity_class_name";
    public static final String MMC_XML_FILE_NAME = "xml_file_name";
    public static final String MMC_XML_FILE_PATH = "xml_file_path";

    public static final String MME_RECORD_ID = "record_id";
    public static final String MME_MAPPER_SIMPLE_CLASS_NAME = "mapper_simple_class_name";
    public static final String MME_ENTITY_SIMPLE_CLASS_NAME = "entity_simple_class_name";
    public static final String MME_TABLE_NAME = "table_name";
    public static final String MME_MAPPER_CLASS_NAME = "mapper_class_name";
    public static final String MME_ENTITY_CLASS_NAME = "entity_class_name";
    public static final String MME_XML_FILE_NAME = "xml_file_name";
    public static final String MME_XML_FILE_PATH = "xml_file_path";

    public static final String SFFMC_RECORD_ID = "record_id";
    public static final String SFFMC_SIMPLE_CLASS_NAME = "simple_class_name";
    public static final String SFFMC_FIELD_NAME = "field_name";
    public static final String SFFMC_SEQ = "seq";
    public static final String SFFMC_CALL_ID = "call_id";
    public static final String SFFMC_FIELD_TYPE_NAD = "field_type_nad";
    public static final String SFFMC_ARRAY_DIMENSIONS = "array_dimensions";
    public static final String SFFMC_CLASS_NAME = "class_name";
    public static final String SFFMC_CALLEE_CLASS_NAME = "callee_class_name";
    public static final String SFFMC_CALLEE_METHOD_NAME = "callee_method_name";

    public static final String FGT_RECORD_ID = "record_id";
    public static final String FGT_SIMPLE_CLASS_NAME = "simple_class_name";
    public static final String FGT_FIELD_NAME = "field_name";
    public static final String FGT_TYPE = "type";
    public static final String FGT_TYPE_SEQ = "type_seq";
    public static final String FGT_SIMPLE_GENERICS_TYPE_NAD = "simple_generics_type_nad";
    public static final String FGT_GENERICS_ARRAY_DIMENSIONS = "generics_array_dimensions";
    public static final String FGT_TYPE_VARIABLES_NAME = "type_variables_name";
    public static final String FGT_WILDCARD = "wildcard";
    public static final String FGT_REFERENCE_TYPE = "reference_type";
    public static final String FGT_GENERICS_CATEGORY = "generics_category";
    public static final String FGT_GENERICS_TYPE_NAD = "generics_type_nad";
    public static final String FGT_CLASS_NAME = "class_name";

    public static final String PC_RECORD_ID = "record_id";
    public static final String PC_PROPERTIES_KEY = "properties_key";
    public static final String PC_PROPERTIES_FILE_PATH = "properties_file_path";
    public static final String PC_PROPERTIES_FILE_NAME = "properties_file_name";
    public static final String PC_PROPERTIES_VALUE = "properties_value";

    public static final String MMSETC_RECORD_ID = "record_id";
    public static final String MMSETC_MAPPER_SIMPLE_CLASS_NAME = "mapper_simple_class_name";
    public static final String MMSETC_MAPPER_METHOD_NAME = "mapper_method_name";
    public static final String MMSETC_TABLE_NAME = COMMON_TABLE_NAME;
    public static final String MMSETC_COLUMN_NAME = COMMON_COLUMN_NAME;
    public static final String MMSETC_PARAM_OBJ_NAME = COMMON_PARAM_OBJ_NAME;
    public static final String MMSETC_PARAM_NAME = COMMON_PARAM_NAME;
    public static final String MMSETC_PARAM_RAW_NAME = COMMON_PARAM_RAW_NAME;
    public static final String MMSETC_MAPPER_CLASS_NAME = "mapper_class_name";
    public static final String MMSETC_XML_FILE_NAME = "xml_file_name";
    public static final String MMSETC_XML_FILE_PATH = "xml_file_path";

    public static final String MMWC_RECORD_ID = "record_id";
    public static final String MMWC_MAPPER_SIMPLE_CLASS_NAME = "mapper_simple_class_name";
    public static final String MMWC_MAPPER_METHOD_NAME = "mapper_method_name";
    public static final String MMWC_TABLE_NAME = COMMON_TABLE_NAME;
    public static final String MMWC_COLUMN_NAME = COMMON_COLUMN_NAME;
    public static final String MMWC_OPERATION = "operation";
    public static final String MMWC_PARAM_OBJ_NAME = COMMON_PARAM_OBJ_NAME;
    public static final String MMWC_PARAM_NAME = COMMON_PARAM_NAME;
    public static final String MMWC_PARAM_RAW_NAME = COMMON_PARAM_RAW_NAME;
    public static final String MMWC_PARAM_TYPE = "param_type";
    public static final String MMWC_MAPPER_CLASS_NAME = "mapper_class_name";
    public static final String MMWC_XML_FILE_NAME = "xml_file_name";
    public static final String MMWC_XML_FILE_PATH = "xml_file_path";

    public static final String MMSELC_RECORD_ID = "record_id";
    public static final String MMSELC_MAPPER_SIMPLE_CLASS_NAME = "mapper_simple_class_name";
    public static final String MMSELC_MAPPER_METHOD_NAME = "mapper_method_name";
    public static final String MMSELC_TABLE_NAME = "table_name";
    public static final String MMSELC_COLUMN_NAME = "column_name";
    public static final String MMSELC_COLUMN_ALIAS = "column_alias";
    public static final String MMSELC_MAPPER_CLASS_NAME = "mapper_class_name";
    public static final String MMSELC_XML_FILE_NAME = "xml_file_name";
    public static final String MMSELC_XML_FILE_PATH = "xml_file_path";

    public static final String MRAS_RECORD_ID = "record_id";
    public static final String MRAS_METHOD_HASH = "method_hash";
    public static final String MRAS_RETURN_ARG_SEQ = "return_arg_seq";
    public static final String MRAS_FULL_METHOD = "full_method";
    public static final String MRAS_RETURN_TYPE = "return_type";
    public static final String MRAS_EQUIVALENT_CONVERSION = "equivalent_conversion";

    public static final String MRCI_RECORD_ID = "record_id";
    public static final String MRCI_METHOD_HASH = "method_hash";
    public static final String MRCI_RETURN_CALL_ID = "return_call_id";
    public static final String MRCI_FULL_METHOD = "full_method";
    public static final String MRCI_RETURN_TYPE = "return_type";
    public static final String MRCI_EQUIVALENT_CONVERSION = "equivalent_conversion";

    public static final String MRCV_RECORD_ID = "record_id";
    public static final String MRCV_METHOD_HASH = "method_hash";
    public static final String MRCV_SEQ = "seq";
    public static final String MRCV_CONST_TYPE = "const_type";
    public static final String MRCV_CONST_VALUE = "const_value";
    public static final String MRCV_FULL_METHOD = "full_method";
    public static final String MRCV_RETURN_TYPE = "return_type";

    public static final String MRFI_RECORD_ID = "record_id";
    public static final String MRFI_METHOD_HASH = "method_hash";
    public static final String MRFI_SEQ = "seq";
    public static final String MRFI_STATIC_FIELD = "static_field";
    public static final String MRFI_FIELD_OF_THIS = "field_of_this";
    public static final String MRFI_FIELD_IN_SIMPLE_CLASS_NAME = "field_in_simple_class_name";
    public static final String MRFI_SIMPLE_FIELD_TYPE_NAD = "simple_field_type_nad";
    public static final String MRFI_FIELD_ARRAY_DIMENSIONS = "field_array_dimensions";
    public static final String MRFI_FIELD_NAME = "field_name";
    public static final String MRFI_FIELD_IN_CLASS_NAME = "field_in_class_name";
    public static final String MRFI_FIELD_TYPE_NAD = "field_type_nad";
    public static final String MRFI_FULL_METHOD = "full_method";
    public static final String MRFI_RETURN_TYPE = "return_type";

    public static final String MMGSD_RECORD_ID = "record_id";
    public static final String MMGSD_FLD_RELATIONSHIP_ID = "fld_relationship_id";
    public static final String MMGSD_GET_OR_SET = "get_or_set";
    public static final String MMGSD_GET_METHOD_CALL_ID = "get_method_call_id";
    public static final String MMGSD_SET_METHOD_CALL_ID = "set_method_call_id";
    public static final String MMGSD_DB_OPERATE = "db_operate";
    public static final String MMGSD_TABLE_NAME = "table_name";
    public static final String MMGSD_COLUMN_NAME = "column_name";
    public static final String MMGSD_COLUMN_RELATE_DESC = "column_relate_desc";

    public static final String MCTH_RECORD_ID = "record_id";
    public static final String MCTH_METHOD_HASH = "method_hash";
    public static final String MCTH_SIMPLE_CLASS_NAME = "simple_class_name";
    public static final String MCTH_METHOD_NAME = "method_name";
    public static final String MCTH_SIMPLE_CATCH_EXCEPTION_TYPE = "simple_catch_exception_type";
    public static final String MCTH_CATCH_EXCEPTION_TYPE = "catch_exception_type";
    public static final String MCTH_CATCH_FLAG = "catch_flag";
    public static final String MCTH_TRY_START_LINE_NUMBER = "try_start_line_number";
    public static final String MCTH_TRY_END_LINE_NUMBER = "try_end_line_number";
    public static final String MCTH_TRY_MIN_CALL_ID = "try_min_call_id";
    public static final String MCTH_TRY_MAX_CALL_ID = "try_max_call_id";
    public static final String MCTH_CATCH_START_OFFSET = "catch_start_offset";
    public static final String MCTH_CATCH_END_OFFSET = "catch_end_offset";
    public static final String MCTH_CATCH_START_LINE_NUMBER = "catch_start_line_number";
    public static final String MCTH_CATCH_END_LINE_NUMBER = "catch_end_line_number";
    public static final String MCTH_CATCH_MIN_CALL_ID = "catch_min_call_id";
    public static final String MCTH_CATCH_MAX_CALL_ID = "catch_max_call_id";
    public static final String MCTH_FULL_METHOD = "full_method";
    public static final String MCTH_RETURN_TYPE = "return_type";

    public static final String MF_RECORD_ID = "record_id";
    public static final String MF_METHOD_HASH = "method_hash";
    public static final String MF_SIMPLE_CLASS_NAME = "simple_class_name";
    public static final String MF_TRY_CATCH = "try_catch";
    public static final String MF_TRY_CATCH_START_LINE_NUMBER = "try_catch_start_line_number";
    public static final String MF_TRY_CATCH_END_LINE_NUMBER = "try_catch_end_line_number";
    public static final String MF_TRY_CATCH_MIN_CALL_ID = "try_catch_min_call_id";
    public static final String MF_TRY_CATCH_MAX_CALL_ID = "try_catch_max_call_id";
    public static final String MF_FINALLY_START_LINE_NUMBER = "finally_start_line_number";
    public static final String MF_FULL_METHOD = "full_method";
    public static final String MF_RETURN_TYPE = "return_type";

    public static final String MT_RECORD_ID = "record_id";
    public static final String MT_METHOD_HASH = "method_hash";
    public static final String MT_SIMPLE_CLASS_NAME = "simple_class_name";
    public static final String MT_THROW_OFFSET = "throw_offset";
    public static final String MT_LINE_NUMBER = "line_number";
    public static final String MT_SEQ = "seq";
    public static final String MT_THROW_EXCEPTION_TYPE = "throw_exception_type";
    public static final String MT_THROW_FLAG = "throw_flag";
    public static final String MT_CATCH_START_OFFSET = "catch_start_offset";
    public static final String MT_CATCH_EXCEPTION_VARIABLE_NAME = "catch_exception_variable_name";
    public static final String MT_CALL_ID = "call_id";
    public static final String MT_FULL_METHOD = "full_method";
    public static final String MT_RETURN_TYPE = "return_type";

    public static final String SMAI_SET_RECORD_ID = "set_record_id";
    public static final String SMAI_SET_METHOD_CALL_ID = "set_method_call_id";
    public static final String SMAI_SEQ = "seq";
    public static final String SMAI_STEP = "step";
    public static final String SMAI_FLD_RELATIONSHIP_ID = "fld_relationship_id";
    public static final String SMAI_CURR_CALL_ID = "curr_call_id";
    public static final String SMAI_CALLER_METHOD_HASH = "caller_method_hash";
    public static final String SMAI_CALLER_FULL_METHOD = "caller_full_method";
    public static final String SMAI_CALLER_LINE_NUMBER = "caller_line_number";
    public static final String SMAI_CALLEE_FULL_METHOD = "callee_full_method";
    public static final String SMAI_SET_METHOD_HASH = "set_method_hash";
    public static final String SMAI_SET_FULL_METHOD = "set_full_method";
    public static final String SMAI_SET_METHOD_IN_SUPER = "set_method_in_super";
    public static final String SMAI_FLAG = "flag";
    public static final String SMAI_FLAG_DESC = "flag_desc";
    public static final String SMAI_ASSIGN_INFO = "assign_info";
    public static final String SMAI_EQUIVALENT_CONVERSION = "equivalent_conversion";

    public static final String MCF_RECORD_ID = "record_id";
    public static final String MCF_CALL_ID = "call_id";
    public static final String MCF_OBJ_ARGS_SEQ = "obj_args_seq";
    public static final String MCF_SEQ = "seq";
    public static final String MCF_CALLER_METHOD_HASH = "caller_method_hash";
    public static final String MCF_SIMPLE_CLASS_NAME = "simple_class_name";
    public static final String MCF_FIELD_NAME = "field_name";
    public static final String MCF_SIMPLE_FIELD_TYPE = "simple_field_type";
    public static final String MCF_CLASS_NAME = "class_name";
    public static final String MCF_FIELD_TYPE = "field_type";

    public static final String[] METHOD_CALL_FIELD_COLUMNS = new String[]{
            MCF_RECORD_ID,
            MCF_CALL_ID,
            MCF_OBJ_ARGS_SEQ,
            MCF_SEQ,
            MCF_CALLER_METHOD_HASH,
            MCF_SIMPLE_CLASS_NAME,
            MCF_FIELD_NAME,
            MCF_SIMPLE_FIELD_TYPE,
            MCF_CLASS_NAME,
            MCF_FIELD_TYPE,
    };

    public static final String MCSFMCR_RECORD_ID = "record_id";
    public static final String MCSFMCR_CALL_ID = "call_id";
    public static final String MCSFMCR_OBJ_ARGS_SEQ = "obj_args_seq";
    public static final String MCSFMCR_SEQ = "seq";
    public static final String MCSFMCR_CALLER_METHOD_HASH = "caller_method_hash";
    public static final String MCSFMCR_SIMPLE_CLASS_NAME = "simple_class_name";
    public static final String MCSFMCR_FIELD_NAME = "field_name";
    public static final String MCSFMCR_SIMPLE_FIELD_TYPE = "simple_field_type";
    public static final String MCSFMCR_CLASS_NAME = "class_name";
    public static final String MCSFMCR_FIELD_TYPE = "field_type";
    public static final String MCSFMCR_CALLEE_METHOD_HASH = "callee_method_hash";
    public static final String MCSFMCR_CALLEE_METHOD_NAME = "callee_method_name";
    public static final String MCSFMCR_CALLEE_FULL_METHOD = "callee_full_method";
    public static final String MCSFMCR_CALLEE_RETURN_TYPE = "callee_return_type";

    public static final String MCMCR_RECORD_ID = "record_id";
    public static final String MCMCR_CALL_ID = "call_id";
    public static final String MCMCR_OBJ_ARGS_SEQ = "obj_args_seq";
    public static final String MCMCR_SEQ = "seq";
    public static final String MCMCR_ARRAY_FLAG = "array_flag";
    public static final String MCMCR_USE_RETURN_CALL_ID = "use_return_call_id";
    public static final String MCMCR_CALLEE_METHOD_HASH = "callee_method_hash";
    public static final String MCMCR_CALLEE_SIMPLE_CLASS_NAME = "callee_simple_class_name";
    public static final String MCMCR_CALLEE_METHOD_NAME = "callee_method_name";
    public static final String MCMCR_CALLEE_FULL_METHOD = "callee_full_method";
    public static final String MCMCR_CALLEE_RETURN_TYPE = "callee_return_type";

    public static final String MMFS_RECORD_ID = "record_id";
    public static final String MMFS_XML_FILE_NAME = "xml_file_name";
    public static final String MMFS_SQL_ID = "sql_id";
    public static final String MMFS_SQL_SEQ = "sql_seq";
    public static final String MMFS_XML_ELEMENT_NAME = "xml_element_name";
    public static final String MMFS_FORMATED_SQL = "formated_sql";
    public static final String MMFS_SQL_HASH = "sql_hash";
    public static final String MMFS_MAPPER_SIMPLE_CLASS_NAME = "mapper_simple_class_name";
    public static final String MMFS_MAPPER_CLASS_NAME = "mapper_class_name";
    public static final String MMFS_XML_FILE_PATH = "xml_file_path";

    private DC() {
        throw new IllegalStateException("illegal");
    }
}
