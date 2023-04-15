package com.adrninistrator.jacg.common;

/**
 * @author adrninistrator
 * @date 2021/6/18
 * @description: 数据库表列名
 */

public class DC {
    public static final String COMMON_CLASS_NAME = "class_name";
    public static final String COMMON_SIMPLE_CLASS_NAME = "simple_class_name";
    public static final String COMMON_METHOD_HASH = "method_hash";
    public static final String COMMON_FULL_METHOD = "full_method";
    public static final String COMMON_CALL_ID = "call_id";
    public static final String COMMON_ANNOTATION_ANNOTATION_NAME = "annotation_name";
    public static final String COMMON_ANNOTATION_ATTRIBUTE_NAME = "attribute_name";
    public static final String COMMON_ANNOTATION_ATTRIBUTE_TYPE = "attribute_type";
    public static final String COMMON_ANNOTATION_ATTRIBUTE_VALUE = "attribute_value";

    public static final String CN_RECORD_ID = "record_id";
    public static final String CN_CLASS_NAME = COMMON_CLASS_NAME;
    public static final String CN_SIMPLE_CLASS_NAME = COMMON_SIMPLE_CLASS_NAME;
    public static final String CN_DUPLICATE_CLASS = "duplicate_class";

    public static final String CA_RECORD_ID = "record_id";
    public static final String CA_SIMPLE_CLASS_NAME = COMMON_SIMPLE_CLASS_NAME;
    public static final String CA_ANNOTATION_NAME = COMMON_ANNOTATION_ANNOTATION_NAME;
    public static final String CA_ATTRIBUTE_NAME = COMMON_ANNOTATION_ATTRIBUTE_NAME;
    public static final String CA_ATTRIBUTE_TYPE = COMMON_ANNOTATION_ATTRIBUTE_TYPE;
    public static final String CA_ATTRIBUTE_VALUE = COMMON_ANNOTATION_ATTRIBUTE_VALUE;
    public static final String CA_CLASS_NAME = COMMON_CLASS_NAME;

    public static final String MA_RECORD_ID = "record_id";
    public static final String MA_METHOD_HASH = COMMON_METHOD_HASH;
    public static final String MA_ANNOTATION_NAME = COMMON_ANNOTATION_ANNOTATION_NAME;
    public static final String MA_ATTRIBUTE_NAME = COMMON_ANNOTATION_ATTRIBUTE_NAME;
    public static final String MA_ATTRIBUTE_TYPE = COMMON_ANNOTATION_ATTRIBUTE_TYPE;
    public static final String MA_ATTRIBUTE_VALUE = COMMON_ANNOTATION_ATTRIBUTE_VALUE;
    public static final String MA_FULL_METHOD = COMMON_FULL_METHOD;
    public static final String MA_SIMPLE_CLASS_NAME = COMMON_SIMPLE_CLASS_NAME;
    public static final String MA_SPRING_MAPPING_ANNOTATION = "spring_mapping_annotation";

    public static final String MC_CALL_ID = COMMON_CALL_ID;
    public static final String MC_CALL_TYPE = "call_type";
    public static final String MC_CALLEE_OBJ_TYPE = "callee_obj_type";
    public static final String MC_ENABLED = "enabled";
    public static final String MC_CALLER_JAR_NUM = "caller_jar_num";
    public static final String MC_CALLER_METHOD_HASH = "caller_method_hash";
    public static final String MC_CALLER_FULL_METHOD = "caller_full_method";
    public static final String MC_CALLER_METHOD_NAME = "caller_method_name";
    public static final String MC_CALLER_SIMPLE_CLASS_NAME = "caller_simple_class_name";
    public static final String MC_CALLER_LINE_NUMBER = "caller_line_number";
    public static final String MC_CALLEE_METHOD_HASH = "callee_method_hash";
    public static final String MC_CALLEE_FULL_METHOD = "callee_full_method";
    public static final String MC_CALLEE_METHOD_NAME = "callee_method_name";
    public static final String MC_CALLEE_SIMPLE_CLASS_NAME = "callee_simple_class_name";
    public static final String MC_CALL_FLAGS = "call_flags";
    public static final String MC_RAW_RETURN_TYPE = "raw_return_type";
    public static final String MC_ACTUAL_RETURN_TYPE = "actual_return_type";

    public static final String LMI_CALL_ID = COMMON_CALL_ID;
    public static final String LMI_LAMBDA_CALLEE_CLASS_NAME = "lambda_callee_class_name";
    public static final String LMI_LAMBDA_CALLEE_METHOD_NAME = "lambda_callee_method_name";
    public static final String LMI_LAMBDA_CALLEE_FULL_METHOD = "lambda_callee_full_method";
    public static final String LMI_LAMBDA_NEXT_CLASS_NAME = "lambda_next_class_name";
    public static final String LMI_LAMBDA_NEXT_METHOD_NAME = "lambda_next_method_name";
    public static final String LMI_LAMBDA_NEXT_FULL_METHOD = "lambda_next_full_method";
    public static final String LMI_LAMBDA_NEXT_IS_STREAM = "lambda_next_is_stream";
    public static final String LMI_LAMBDA_NEXT_IS_INTERMEDIATE = "lambda_next_is_intermediate";
    public static final String LMI_LAMBDA_NEXT_IS_TERMINAL = "lambda_next_is_terminal";

    public static final String MLN_METHOD_HASH = COMMON_METHOD_HASH;
    public static final String MLN_MIN_LINE_NUMBER = "min_line_number";
    public static final String MLN_MAX_LINE_NUMBER = "max_line_number";
    public static final String MLN_SIMPLE_CLASS_NAME = COMMON_SIMPLE_CLASS_NAME;
    public static final String MLN_FULL_METHOD = COMMON_FULL_METHOD;

    public static final String JI_JAR_NUM = "jar_num";
    public static final String JI_JAR_TYPE = "jar_type";
    public static final String JI_JAR_PATH_HASH = "jar_path_hash";
    public static final String JI_JAR_FULL_PATH = "jar_full_path";
    public static final String JI_LAST_MODIFIED = "last_modified";
    public static final String JI_JAR_HASH = "jar_hash";

    public static final String BD_CALL_ID = COMMON_CALL_ID;
    public static final String BD_DATA_TYPE = "data_type";
    public static final String BD_DATA_VALUE = "data_value";

    public static final String CI_RECORD_ID = "record_id";
    public static final String CI_SIMPLE_CLASS_NAME = COMMON_SIMPLE_CLASS_NAME;
    public static final String CI_ACCESS_FLAGS = "access_flags";
    public static final String CI_CLASS_NAME = COMMON_CLASS_NAME;

    public static final String MI_METHOD_HASH = COMMON_METHOD_HASH;
    public static final String MI_SIMPLE_CLASS_NAME = COMMON_SIMPLE_CLASS_NAME;
    public static final String MI_ACCESS_FLAGS = "access_flags";
    public static final String MI_METHOD_NAME = "method_name";
    public static final String MI_FULL_METHOD = COMMON_FULL_METHOD;
    public static final String MI_SIMPLE_RETURN_TYPE = "simple_return_type";
    public static final String MI_RETURN_TYPE = "return_type";

    public static final String EI_RECORD_ID = "record_id";
    public static final String EI_SIMPLE_CLASS_NAME = COMMON_SIMPLE_CLASS_NAME;
    public static final String EI_CLASS_NAME = COMMON_CLASS_NAME;
    public static final String EI_ACCESS_FLAGS = "access_flags";
    public static final String EI_TYPE = "type";
    public static final String EI_SEQ = "seq";
    public static final String EI_EXISTS_DOWNWARD_CLASSES = "exists_downward_classes";
    public static final String EI_UPWARD_SIMPLE_CLASS_NAME = "upward_simple_class_name";
    public static final String EI_UPWARD_CLASS_NAME = "upward_class_name";

    public static final String MCI_CALL_ID = COMMON_CALL_ID;
    public static final String MCI_OBJ_ARGS_SEQ = "obj_args_seq";
    public static final String MCI_SEQ = "seq";
    public static final String MCI_TYPE = "type";
    // H2数据库中array是关键字不能使用
    public static final String MCI_ARRAY_FLAG = "array_flag";
    // H2中使用"value"作为字段名会报错
    public static final String MCI_THE_VALUE = "the_value";

    public static final String SPB_RECORD_ID = "record_id";
    public static final String SPB_SPRING_BEAN_NAME = "spring_bean_name";
    public static final String SPB_SEQ = "seq";
    public static final String SPB_CLASS_NAME = COMMON_CLASS_NAME;

    public static final String SPC_METHOD_HASH = COMMON_METHOD_HASH;
    public static final String SPC_SEQ = "seq";
    public static final String SPC_SHOW_URI = "show_uri";
    public static final String SPC_CLASS_PATH = "class_path";
    public static final String SPC_METHOD_PATH = "method_path";
    public static final String SPC_ANNOTATION_ANNOTATION_NAME = COMMON_ANNOTATION_ANNOTATION_NAME;
    public static final String SPC_SIMPLE_CLASS_NAME = COMMON_SIMPLE_CLASS_NAME;
    public static final String SPC_FULL_METHOD = COMMON_FULL_METHOD;

    public static final String SPT_RECORD_ID = "record_id";
    public static final String SPT_SPRING_BEAN_NAME = "spring_bean_name";
    public static final String SPT_CLASS_NAME = COMMON_CLASS_NAME;
    public static final String SPT_METHOD_NAME = "method_name";

    public static final String CSEI1_RECORD_ID = "record_id";
    public static final String CSEI1_SIMPLE_CLASS_NAME = "simple_class_name";
    public static final String CSEI1_TYPE = "type";
    public static final String CSEI1_SUPER_ITF_CLASS_NAME = "super_itf_class_name";
    public static final String CSEI1_SEQ = "seq";
    public static final String CSEI1_SIGN_CLASS_NAME = "sign_class_name";
    public static final String CSEI1_CLASS_NAME = "class_name";

    public static final String MMT_RECORD_ID = "record_id";
    public static final String MMT_MAPPER_SIMPLE_CLASS_NAME = "mapper_simple_class_name";
    public static final String MMT_MAPPER_METHOD_NAME = "mapper_method_name";
    public static final String MMT_SQL_STATEMENT = "sql_statement";
    public static final String MMT_TABLE_SEQ = "table_seq";
    public static final String MMT_TABLE_NAME = "table_name";
    public static final String MMT_MAPPER_CLASS_NAME = "mapper_class_name";

    public static final String MMWT_RECORD_ID = "record_id";
    public static final String MMWT_MAPPER_SIMPLE_CLASS_NAME = "mapper_simple_class_name";
    public static final String MMWT_MAPPER_METHOD_NAME = "mapper_method_name";
    public static final String MMWT_SQL_STATEMENT = "sql_statement";
    public static final String MMWT_TABLE_NAME = "table_name";
    public static final String MMWT_MAPPER_CLASS_NAME = "mapper_class_name";

    public static final String ACP_RECORD_ID = "record_id";
    public static final String ACP_CLASS_PREFIX = "class_prefix";

    public static final String MAGT_RECORD_ID = "record_id";
    public static final String MAGT_METHOD_HASH = "method_hash";
    public static final String MAGT_SIMPLE_CLASS_NAME = "simple_class_name";
    public static final String MAGT_ARG_SEQ = "arg_seq";
    public static final String MAGT_TYPE = "type";
    public static final String MAGT_TYPE_SEQ = "type_seq";
    public static final String MAGT_SIMPLE_GENERICS_TYPE = "simple_generics_type";
    public static final String MAGT_GENERICS_TYPE = "generics_type";
    public static final String MAGT_FULL_METHOD = "full_method";

    public static final String MAT_METHOD_HASH = "method_hash";
    public static final String MAT_ARG_SEQ = "arg_seq";
    public static final String MAT_SIMPLE_ARG_TYPE = "simple_arg_type";
    public static final String MAT_ARG_TYPE = "arg_type";
    public static final String MAT_SIMPLE_CLASS_NAME = "simple_class_name";
    public static final String MAT_FULL_METHOD = "full_method";

    public static final String MRGT_RECORD_ID = "record_id";
    public static final String MRGT_METHOD_HASH = "method_hash";
    public static final String MRGT_SIMPLE_CLASS_NAME = "simple_class_name";
    public static final String MRGT_TYPE = "type";
    public static final String MRGT_TYPE_SEQ = "type_seq";
    public static final String MRGT_SIMPLE_GENERICS_TYPE = "simple_generics_type";
    public static final String MRGT_GENERICS_TYPE = "generics_type";
    public static final String MRGT_FULL_METHOD = "full_method";

    public static final String IC_SIMPLE_INNER_CLASS_NAME = "simple_inner_class_name";
    public static final String IC_INNER_CLASS_NAME = "inner_class_name";
    public static final String IC_SIMPLE_OUTER_CLASS_NAME = "simple_outer_class_name";
    public static final String IC_OUTER_CLASS_NAME = "outer_class_name";
    public static final String IC_ANONYMOUS_CLASS = "anonymous_class";

    private DC() {
        throw new IllegalStateException("illegal");
    }
}
