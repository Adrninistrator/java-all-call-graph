package com.adrninistrator.jacg.common;

/**
 * @author adrninistrator
 * @date 2021/6/18
 * @description:
 */

public class DC {
    public static final String COMMON_ANNOTATION_ANNOTATION_NAME = "annotation_name";
    public static final String COMMON_ANNOTATION_ATTRIBUTE_NAME = "attribute_name";
    public static final String COMMON_ANNOTATION_ATTRIBUTE_VALUE = "attribute_value";

    public static final String ALIAS_ANNOTATION_CLASS_OR_METHOD = "class_or_method";

    public static final String CN_FULL_NAME = "full_name";
    public static final String CN_SIMPLE_NAME = "simple_name";

    public static final String CA_FULL_CLASS_NAME = "full_class_name";
    public static final String CA_ANNOTATION_NAME = COMMON_ANNOTATION_ANNOTATION_NAME;
    public static final String CA_ATTRIBUTE_NAME = COMMON_ANNOTATION_ATTRIBUTE_NAME;
    public static final String CA_ATTRIBUTE_VALUE = COMMON_ANNOTATION_ATTRIBUTE_VALUE;

    public static final String MA_METHOD_HASH = "method_hash";
    public static final String MA_ANNOTATION_NAME = COMMON_ANNOTATION_ANNOTATION_NAME;
    public static final String MA_ATTRIBUTE_NAME = COMMON_ANNOTATION_ATTRIBUTE_NAME;
    public static final String MA_ATTRIBUTE_VALUE = COMMON_ANNOTATION_ATTRIBUTE_VALUE;
    public static final String MA_FULL_METHOD = "full_method";

    public static final String MC_CALL_ID = "call_id";
    public static final String MC_CALL_TYPE = "call_type";
    public static final String MC_ENABLED = "enabled";
    public static final String MC_CALLER_JAR_NUM = "caller_jar_num";
    public static final String MC_CALLER_METHOD_HASH = "caller_method_hash";
    public static final String MC_CALLER_FULL_METHOD = "caller_full_method";
    public static final String MC_CALLER_METHOD_NAME = "caller_method_name";
    public static final String MC_CALLER_FULL_CLASS_NAME = "caller_full_class_name";
    public static final String MC_CALLER_CLASS_NAME = "caller_class_name";
    public static final String MC_CALLER_LINE_NUM = "caller_line_num";
    public static final String MC_CALLEE_METHOD_HASH = "callee_method_hash";
    public static final String MC_CALLEE_FULL_METHOD = "callee_full_method";
    public static final String MC_CALLEE_METHOD_NAME = "callee_method_name";
    public static final String MC_CALLEE_FULL_CLASS_NAME = "callee_full_class_name";
    public static final String MC_CALLEE_CLASS_NAME = "callee_class_name";

    public static final String MLN_METHOD_HASH = "method_hash";
    public static final String MLN_MIN_LINE_NUMBER = "min_line_number";
    public static final String MLN_MAX_LINE_NUMBER = "max_line_number";
    public static final String MLN_SIMPLE_CLASS_NAME = "simple_class_name";
    public static final String MLN_FULL_METHOD = "full_method";

    public static final String JI_JAR_NUM = "jar_num";
    public static final String JI_JAR_TYPE = "jar_type";
    public static final String JI_JAR_PATH_HASH = "jar_path_hash";
    public static final String JI_JAR_FULL_PATH = "jar_full_path";
    public static final String JI_LAST_MODIFIED = "last_modified";
    public static final String JI_JAR_HASH = "jar_hash";

    public static final String ED_CALL_ID = "call_id";
    public static final String ED_DATA_TYPE = "data_type";
    public static final String ED_DATA_VALUE = "data_value";

    public static final String MAED_DATA_ID = "data_id";
    public static final String MAED_CALLER_FULL_METHOD = "caller_full_method";
    public static final String MAED_CALLEE_FULL_METHOD = "callee_full_method";
    public static final String MAED_CALLEE_SEQ_IN_CALLER = "callee_seq_in_caller";
    public static final String MAED_DATA_TYPE = "data_type";
    public static final String MAED_DATA_VALUE = "data_value";

    private DC() {
        throw new IllegalStateException("illegal");
    }
}
