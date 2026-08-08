package com.adrninistrator.jacg.common.enums;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.JACGConstants;

/**
 * @author adrninistrator
 * @date 2022/11/16
 * @description: 数据库表信息枚举
 */
public enum DbTableInfoEnum {
    DTIE_CONFIG("config", new String[]{
            DC.CONFIG_FILE_NAME,
            DC.CONFIG_KEY,
            DC.CONFIG_VALUE,
            DC.CONFIG_TYPE,
    }, "组件使用的配置参数，记录java-callgraph2与java-all-call-graph执行时使用的配置键、值、类型及来源文件"),
    DTIE_BUSINESS_DATA("business_data", new String[]{
            DC.BD_CALL_ID,
            DC.BD_DATA_TYPE,
            DC.BD_DATA_VALUE,
    }, "业务自定义数据，按方法调用ID记录自定义的业务数据类型与值，供业务扩展使用"),
    DTIE_PARSED_CUSTOM_DATA("parsed_custom_data", new String[]{
            DC.PCD_RECORD_ID,
            DC.PCD_DATA_TYPE,
            DC.PCD_DATA_KEY,
            DC.PCD_DATA_VALUE,
    }, "解析jar时获取的自定义数据，按数据类型、key记录解析得到的自定义数据值"),
    DTIE_CLASS_ANNOTATION("class_annotation", new String[]{
            DC.CA_RECORD_ID,
            DC.CA_SIMPLE_CLASS_NAME,
            DC.CA_ANNOTATION_NAME,
            DC.CA_ATTRIBUTE_NAME,
            DC.CA_ATTRIBUTE_TYPE,
            DC.CA_ATTRIBUTE_VALUE,
            DC.CA_CLASS_NAME,
    }, "类的注解信息，记录每个类上的注解名称、属性名/类型/值及所属类"),
    DTIE_CLASS_INFO("class_info", DC.CLASS_INFO_COLUMNS,
            "类的信息，记录每个类的完整类名、唯一类名、access flags、是否接口/枚举/抽象等"),
    DTIE_DUP_CLASS_INFO("dup_class_info", DC.CLASS_INFO_COLUMNS,
            "重复同名类的信息，记录包名不同但简单类名相同的类信息，用于区分同名类"),
    DTIE_CLASS_NAME("class_name", new String[]{
            DC.CN_RECORD_ID,
            DC.CN_CLASS_NAME,
            DC.CN_SIMPLE_CLASS_NAME,
            DC.CN_DUPLICATE_CLASS,
    }, "类名映射，记录完整类名、唯一类名（去重后的简单类名）、是否为重复同名类，用于按唯一类名定位类"),
    DTIE_CLASS_REFERENCE("class_reference", DC.CLASS_REFERENCE_COLUMNS,
            "类引用关系，记录每个类中引用的其他类（被引用的类）"),
    DTIE_DUP_CLASS_REFERENCE("dup_class_reference", DC.CLASS_REFERENCE_COLUMNS,
            "重复同名类的引用关系，记录重复同名类中引用的其他类"),
    DTIE_CLASS_SIGNATURE_GENERICS_TYPE("class_signature_generics_type", new String[]{
            DC.CSGT_RECORD_ID,
            DC.CSGT_SIMPLE_CLASS_NAME,
            DC.CSGT_SEQ,
            DC.CSGT_TYPE_VARIABLES_NAME,
            DC.CSGT_GENERICS_EXTENDS_CLASS_NAME,
            DC.CSGT_CLASS_NAME,
    }, "类签名中的泛型信息，记录类签名中定义的类型变量名及其泛型上界类名"),
    DTIE_CLASS_EXT_IMPL_GENERICS_TYPE("class_ext_impl_generics_type", new String[]{
            DC.CEIGT_RECORD_ID,
            DC.CEIGT_SIMPLE_CLASS_NAME,
            DC.CEIGT_EXT_TYPE,
            DC.CEIGT_SEQ,
            DC.CEIGT_SUPER_ITF_SIMPLE_CLASS_NAME,
            DC.CEIGT_GENERICS_SEQ,
            DC.CEIGT_SIMPLE_GENERICS_TYPE_NAD,
            DC.CEIGT_GENERICS_ARRAY_DIMENSIONS,
            DC.CEIGT_TYPE_VARIABLES_NAME,
            DC.CEIGT_GENERICS_CATEGORY,
            DC.CEIGT_GENERICS_TYPE_NAD,
            DC.CEIGT_CLASS_NAME,
            DC.CEIGT_SUPER_ITF_CLASS_NAME,
    }, "类的继承或实现关系中涉及的泛型信息，记录父类/接口泛型参数的类型、数组维度、通配符等"),
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
    }, "类的继承与实现关系，记录每个类的父类与接口（向上）、access flags、是否存在向下子类等"),
    DTIE_ENUM_INIT_ARG_FIELD("enum_init_arg_field", new String[]{
            DC.EIAF_RECORD_ID,
            DC.EIAF_SIMPLE_CLASS_NAME,
            DC.EIAF_ARG_SEQ,
            DC.EIAF_FIELD_TYPE,
            DC.EIAF_FIELD_NAME,
            DC.EIAF_CLASS_NAME,
            DC.EIAF_FULL_METHOD,
    }, "枚举类构造函数参数与字段赋值关系，记录枚举常量构造参数序号对应的字段类型与名称"),
    DTIE_ENUM_INIT_ASSIGN_INFO("enum_init_assign_info", new String[]{
            DC.EIAI_RECORD_ID,
            DC.EIAI_SIMPLE_CLASS_NAME,
            DC.EIAI_CONST_NAME,
            DC.EIAI_ORDINAL,
            DC.EIAI_ARG_SEQ,
            DC.EIAI_FIELD_TYPE,
            DC.EIAI_FIELD_VALUE,
            DC.EIAI_CLASS_NAME,
            DC.EIAI_FULL_METHOD,
    }, "枚举类初始化赋值信息，记录每个枚举常量的名称、序号、构造参数对应的字段类型与赋值"),
    DTIE_EXTENDS_IMPL_PRE("extends_impl!pre", null, ""),
    DTIE_INNER_CLASS("inner_class", new String[]{
            DC.IC_INNER_SIMPLE_CLASS_NAME,
            DC.IC_INNER_CLASS_NAME,
            DC.IC_OUTER_SIMPLE_CLASS_NAME,
            DC.IC_OUTER_CLASS_NAME,
            DC.IC_ANONYMOUS_CLASS,
    }, "内部类信息，记录内部类与外部类的对应关系及是否匿名类"),
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
            DC.JI_INNER_JAR_PATH,
            DC.JI_INNER_JAR_FILE_NAME,
            DC.JI_IMPORT_TIME,
    }, "jar文件信息，记录每个jar的序号、类型、完整路径、文件名、哈希、修改时间及导入时间"),
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
    }, "Lambda表达式方法信息，记录Lambda生成的方法及内部下一个调用方法、是否Stream中间/终止操作"),
    DTIE_METHOD_ANNOTATION("method_annotation", new String[]{
            DC.MA_RECORD_ID,
            DC.MA_METHOD_HASH,
            DC.MA_ANNOTATION_NAME,
            DC.MA_ATTRIBUTE_NAME,
            DC.MA_ATTRIBUTE_TYPE,
            DC.MA_ATTRIBUTE_VALUE,
            DC.MA_FULL_METHOD,
            DC.MA_RETURN_TYPE,
            DC.MA_JAR_NUM,
            DC.MA_SIMPLE_CLASS_NAME,
    }, "方法注解信息，记录每个方法上的注解名称、属性名/类型/值及所属方法"),
    DTIE_METHOD_ARG_ANNOTATION("method_arg_annotation", new String[]{
            DC.MAA_RECORD_ID,
            DC.MAA_METHOD_HASH,
            DC.MAA_ARG_SEQ,
            DC.MAA_ANNOTATION_NAME,
            DC.MAA_ATTRIBUTE_NAME,
            DC.MAA_ATTRIBUTE_TYPE,
            DC.MAA_ATTRIBUTE_VALUE,
            DC.MAA_FULL_METHOD,
            DC.MAA_RETURN_TYPE,
            DC.MAA_SIMPLE_CLASS_NAME,
    }, "方法参数注解信息，记录方法每个参数上的注解及属性、所属方法"),
    DTIE_METHOD_ARG_GENERICS_TYPE("method_arg_generics_type", new String[]{
            DC.MAGT_RECORD_ID,
            DC.MAGT_METHOD_HASH,
            DC.MAGT_SIMPLE_CLASS_NAME,
            DC.MAGT_SEQ,
            DC.MAGT_TYPE,
            DC.MAGT_TYPE_SEQ,
            DC.MAGT_SIMPLE_GENERICS_TYPE_NAD,
            DC.MAGT_GENERICS_ARRAY_DIMENSIONS,
            DC.MAGT_TYPE_VARIABLES_NAME,
            DC.MAGT_WILDCARD,
            DC.MAGT_REFERENCE_TYPE,
            DC.MAGT_GENERICS_CATEGORY,
            DC.MAGT_GENERICS_TYPE_NAD,
            DC.MAGT_FULL_METHOD,
            DC.MAGT_RETURN_TYPE,
    }, "方法参数集合中涉及的泛型类型，记录参数泛型的类型、数组维度、通配符、引用类型等"),
    DTIE_METHOD_ARGUMENT("method_argument", new String[]{
            DC.MARG_RECORD_ID,
            DC.MARG_METHOD_HASH,
            DC.MARG_ARG_SEQ,
            DC.MARG_SIMPLE_ARG_TYPE_NAD,
            DC.MARG_ARG_NAME,
            DC.MARG_ARG_TYPE_NAD,
            DC.MARG_ARRAY_DIMENSIONS,
            DC.MARG_ARG_CATEGORY,
            DC.MARG_EXISTS_GENERICS_TYPE,
            DC.MARG_SIMPLE_CLASS_NAME,
            DC.MARG_FULL_METHOD,
            DC.MARG_RETURN_TYPE,
    }, "方法参数信息，记录每个方法参数的序号、名称、类型、数组维度、是否含泛型等"),
    DTIE_METHOD_CALL("method_call", new String[]{
            DC.MC_CALL_ID,
            DC.MC_ENABLED,
            DC.MC_CALL_TYPE,
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
            DC.MC_CALLEE_ARRAY_DIMENSIONS,
            DC.MC_CALLEE_OBJ_TYPE,
            DC.MC_RAW_RETURN_TYPE,
            DC.MC_ACTUAL_RETURN_TYPE,
            DC.MC_CALL_FLAGS,
            DC.MC_CALLER_JAR_NUM,
            DC.MC_CALLEE_JAR_NUM,
            DC.MC_DESCRIPTION,
    }, "方法调用关系，记录每次方法调用的调用方与被调用方（完整方法、hash、行号）、调用类型、被调用对象类型、返回类型等，是调用图核心数据"),
    DTIE_METHOD_CALL_RAW_CALLEE("method_call_raw_callee", new String[]{
            DC.MCRC_CALL_ID,
            DC.MCRC_RAW_CALLEE_CLASS_NAME,
    }, "方法调用被调用对象的原始类型，当原始被调用对象与实际不同时记录原始完整类名"),
    DTIE_METHOD_CALL_INFO("method_call_info", new String[]{
            DC.MCI_RECORD_ID,
            DC.MCI_CALL_ID,
            DC.MCI_OBJ_ARGS_SEQ,
            DC.MCI_SEQ,
            DC.MCI_TYPE,
            DC.MCI_ARRAY_FLAG,
            DC.MCI_ARRAY_COLLECTION_SEQ,
            DC.MCI_ARRAY_DIMENSIONS,
            DC.MCI_ARRAY_INDEX,
            DC.MCI_VALUE_TYPE,
            DC.MCI_THE_VALUE,
            DC.MCI_CALLER_METHOD_HASH,
    }, "方法调用中被调用对象与参数可能的类型及值，含数组/集合元素、常量值，用于分析运行时实际类型与值"),
    DTIE_METHOD_CALL_STATIC_FIELD("method_call_static_field", DC.METHOD_CALL_FIELD_COLUMNS,
            "方法调用使用静态字段信息，记录方法调用中被调用对象或参数使用的静态字段（类名、字段名、类型）"),
    DTIE_METHOD_CALL_NON_STATIC_FIELD("method_call_non_static_field", DC.METHOD_CALL_FIELD_COLUMNS,
            "方法调用使用非静态字段信息，记录方法调用中被调用对象或参数使用的非静态字段（所在类、字段名、类型）"),
    DTIE_METHOD_CALL_FIELD_ACTUAL_TYPE("method_call_field_actual_type", new String[]{
            DC.MCFAT_RECORD_ID,
            DC.MCFAT_CALLER_SIMPLE_CLASS_NAME,
            DC.MCFAT_CALLER_CLASS_NAME,
            DC.MCFAT_CALLER_METHOD_NAME,
            DC.MCFAT_CALLER_FULL_METHOD,
            DC.MCFAT_CALLER_LINE_NUMBER,
            DC.MCFAT_FIELD_TYPE,
            DC.MCFAT_FIELD_NAME,
            DC.MCFAT_FIELD_ACTUAL_TYPE,
    }, "方法调用被调用对象为非静态字段时的运行时实际类型（多态），仅当字段声明类型与实际类型不同时记录，每个实际类型一行"),
    DTIE_METHOD_CALL_STATIC_FIELD_MCR("method_call_static_field_mcr", new String[]{
            DC.MCSFMCR_RECORD_ID,
            DC.MCSFMCR_CALL_ID,
            DC.MCSFMCR_OBJ_ARGS_SEQ,
            DC.MCSFMCR_SEQ,
            DC.MCSFMCR_CALLER_METHOD_HASH,
            DC.MCSFMCR_SIMPLE_CLASS_NAME,
            DC.MCSFMCR_FIELD_NAME,
            DC.MCSFMCR_SIMPLE_FIELD_TYPE,
            DC.MCSFMCR_CLASS_NAME,
            DC.MCSFMCR_FIELD_TYPE,
            DC.MCSFMCR_CALLEE_METHOD_HASH,
            DC.MCSFMCR_CALLEE_METHOD_NAME,
            DC.MCSFMCR_CALLEE_FULL_METHOD,
            DC.MCSFMCR_CALLEE_RETURN_TYPE,
    }, "方法调用中使用静态字段的方法调用返回值信息，记录被调用对象或参数为静态字段且其值为方法调用返回值的情况"),
    DTIE_METHOD_INFO("method_info", DC.METHOD_INFO_COLUMNS,
            "方法信息，记录每个方法的完整方法、hash、返回类型、所属类、access flags等"),
    DTIE_DUP_METHOD_INFO("dup_method_info", DC.METHOD_INFO_COLUMNS,
            "重复同名类的方法信息，记录包名不同但简单类名相同的类中的方法信息"),
    DTIE_METHOD_LINE_NUMBER("method_line_number", new String[]{
            DC.MLN_RECORD_ID,
            DC.MLN_METHOD_HASH,
            DC.MLN_SIMPLE_CLASS_NAME,
            DC.MLN_METHOD_NAME,
            DC.MLN_MIN_LINE_NUMBER,
            DC.MLN_MAX_LINE_NUMBER,
            DC.MLN_FULL_METHOD,
            DC.MLN_RETURN_TYPE
    }, "方法代码行号，记录每个方法的最小与最大代码行号，用于按行号定位方法"),
    DTIE_METHOD_RETURN_GENERICS_TYPE("method_return_generics_type", new String[]{
            DC.MRGT_RECORD_ID,
            DC.MRGT_METHOD_HASH,
            DC.MRGT_SIMPLE_CLASS_NAME,
            DC.MRGT_TYPE,
            DC.MRGT_TYPE_SEQ,
            DC.MRGT_SIMPLE_GENERICS_TYPE_NAD,
            DC.MRGT_GENERICS_ARRAY_DIMENSIONS,
            DC.MRGT_TYPE_VARIABLES_NAME,
            DC.MRGT_WILDCARD,
            DC.MRGT_REFERENCE_TYPE,
            DC.MRGT_GENERICS_CATEGORY,
            DC.MRGT_GENERICS_TYPE_NAD,
            DC.MRGT_FULL_METHOD,
            DC.MRGT_RETURN_TYPE,
    }, "方法返回值集合中涉及的泛型类型，记录返回值泛型的类型、数组维度、通配符等"),
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
    }, "MyBatis Mapper方法涉及的表，记录Mapper方法、SQL语句类型、表名及XML来源"),
    DTIE_MYBATIS_MS_WRITE_TABLE("mybatis_ms_write_table", new String[]{
            DC.MMWT_RECORD_ID,
            DC.MMWT_MAPPER_SIMPLE_CLASS_NAME,
            DC.MMWT_MAPPER_METHOD_NAME,
            DC.MMWT_SQL_STATEMENT,
            DC.MMWT_TABLE_NAME,
            DC.MMWT_MAPPER_CLASS_NAME,
            DC.MMWT_XML_FILE_NAME,
            DC.MMWT_XML_FILE_PATH,
    }, "MyBatis Mapper方法写入的表，记录执行insert/update/delete的Mapper方法与目标表"),
    DTIE_SF_FIELD_METHOD_CALL("sf_field_method_call", new String[]{
            DC.SFFMC_RECORD_ID,
            DC.SFFMC_SIMPLE_CLASS_NAME,
            DC.SFFMC_FIELD_NAME,
            DC.SFFMC_SEQ,
            DC.SFFMC_CALL_ID,
            DC.SFFMC_FIELD_TYPE_NAD,
            DC.SFFMC_ARRAY_DIMENSIONS,
            DC.SFFMC_CLASS_NAME,
            DC.SFFMC_CALLEE_CLASS_NAME,
            DC.SFFMC_CALLEE_METHOD_NAME,
    }, "static、final字段初始化方法信息（含枚举），记录静态final字段初始化时调用的方法及被调用方法"),
    DTIE_SPRING_BEAN("spring_bean", new String[]{
            DC.SPB_RECORD_ID,
            DC.SPB_SPRING_BEAN_NAME,
            DC.SPB_SEQ,
            DC.SPB_SIMPLE_CLASS_NAME,
            DC.SPB_CLASS_NAME,
            DC.SPB_PROFILE,
            DC.SPB_BEAN_TYPE,
            DC.SPB_ANNOTATION_CLASS_NAME,
            DC.SPB_DEFINE_CLASS_NAME_XML_PATH,
    }, "Spring Bean信息，记录每个Bean的名称、类型、序号、profile、来源（注解/XML）等"),
    DTIE_SPRING_DI_FIELD("spring_di_field", new String[]{
            DC.SDI_RECORD_ID,
            DC.SDI_CLASS_NAME,
            DC.SDI_SIMPLE_CLASS_NAME,
            DC.SDI_FIELD_TYPE,
            DC.SDI_SIMPLE_FIELD_TYPE,
            DC.SDI_FIELD_NAME,
            DC.SDI_BEAN_TYPE,
            DC.SDI_SIMPLE_BEAN_TYPE,
            DC.SDI_SAME_TYPE,
    }, "Spring依赖注入字段信息，记录注入字段的声明类型、Bean实际类型、是否相同，用于分析注入字段运行时类型"),
    DTIE_SPRING_CONTROLLER("spring_controller", new String[]{
            DC.SPC_RECORD_ID,
            DC.SPC_METHOD_HASH,
            DC.SPC_SEQ,
            DC.SPC_SHOW_URI,
            DC.SPC_CLASS_PATH,
            DC.SPC_METHOD_PATH,
            DC.SPC_ANNOTATION_ANNOTATION_NAME,
            DC.SPC_SIMPLE_CLASS_NAME,
            DC.SPC_JAR_NUM,
            DC.SPC_MAYBE_FILE_UPLOAD,
            DC.SPC_MAYBE_FILE_DOWNLOAD,
            DC.SPC_FULL_METHOD,
            DC.SPC_RETURN_TYPE,
    }, "Spring Controller信息，记录Controller方法对应的请求URI、类/方法路径、注解、是否文件上传/下载等"),
    DTIE_SPRING_SCAN_PACKAGE("spring_scan_package", new String[]{
            DC.SPSP_RECORD_ID,
            DC.SPSP_TYPE,
            DC.SPSP_SEQ,
            DC.SPSP_SCAN_PACKAGE,
            DC.SPSP_DEFINE_CLASS_NAME_XML_PATH,
    }, "Spring包扫描路径，记录Java代码或XML中定义的组件扫描包路径"),
    DTIE_SPRING_TASK("spring_task", new String[]{
            DC.SPT_RECORD_ID,
            DC.SPT_METHOD_HASH,
            DC.SPT_SPRING_BEAN_NAME,
            DC.SPT_CLASS_NAME,
            DC.SPT_METHOD_NAME,
            DC.SPT_TYPE,
            DC.SPT_FULL_METHOD,
            DC.SPT_RETURN_TYPE,
            DC.SPT_DEFINE_CLASS_NAME_XML_PATH,
    }, "Spring定时任务，记录@Scheduled等方法及来源（注解/XML）"),
    DTIE_SPRING_AOP_ASPECT("spring_aop_aspect", new String[]{
            DC.SAAS_RECORD_ID,
            DC.SAAS_TYPE,
            DC.SAAS_XML_ASPECT_ID,
            DC.SAAS_XML_ASPECT_REF,
            DC.SAAS_ASPECT_ORDER,
            DC.SAAS_CLASS_NAME,
            DC.SAAS_DEFINE_XML_PATH,
    }, "Spring AOP Aspect，记录切面类、顺序及来源（注解/XML）"),
    DTIE_SPRING_AOP_POINTCUT("spring_aop_pointcut", new String[]{
            DC.SAP_RECORD_ID,
            DC.SAP_TYPE,
            DC.SAP_XML_POINTCUT_ID,
            DC.SAP_EXPRESSION,
            DC.SAP_FULL_METHOD,
            DC.SAP_DEFINE_XML_PATH
    }, "Spring AOP Pointcut，记录切点表达式及来源"),
    DTIE_SPRING_AOP_ADVICE("spring_aop_advice", new String[]{
            DC.SAAD_RECORD_ID,
            DC.SAAD_TYPE,
            DC.SAAD_XML_ASPECT_ID,
            DC.SAAD_XML_ASPECT_METHOD_NAME,
            DC.SAAD_ADVICE_TYPE,
            DC.SAAD_XML_POINTCUT_REF,
            DC.SAAD_EXPRESSION,
            DC.SAAD_ASPECT_ORDER,
            DC.SAAD_ADVICE_FULL_METHOD,
            DC.SAAD_ADVICE_METHOD_RETURN_TYPE,
            DC.SAAD_ADVICE_METHOD_HASH,
            DC.SAAD_ASPECT_CLASS_NAME,
            DC.SAAD_DEFINE_XML_PATH
    }, "Spring AOP Advice，记录通知（Before/After/Around等）对应的方法、切点、切面类及来源"),
    DTIE_SPRING_AOP_ADVICE_AROUND("spring_aop_advice_around", new String[]{
            DC.SAADA_RECORD_ID,
            DC.SAADA_ADVICE_FULL_METHOD,
            DC.SAADA_ADVICE_METHOD_RETURN_TYPE,
            DC.SAAVA_ADVICE_METHOD_HASH,
            DC.SAADA_PROCEED_CALL_ID,
    }, "Spring AOP Around通知，记录Around通知方法及内部调用ProceedingJoinPoint.proceed()的方法调用ID"),
    DTIE_SPRING_AOP_ADVICE_AFFECTED_METHOD("spring_aop_advice_affected_method", new String[]{
            DC.SAADAM_RECORD_ID,
            DC.SAADAM_TYPE,
            DC.SAADAM_XML_ASPECT_ID,
            DC.SAADAM_XML_ASPECT_METHOD_NAME,
            DC.SAADAM_ADVICE_TYPE,
            DC.SAADAM_XML_POINTCUT_REF,
            DC.SAADAM_EXPRESSION,
            DC.SAADAM_ASPECT_ORDER,
            DC.SAADAM_ADVICE_FULL_METHOD,
            DC.SAADAM_ADVICE_METHOD_RETURN_TYPE,
            DC.SAADAM_ADVICE_METHOD_HASH,
            DC.SAADAM_ASPECT_CLASS_NAME,
            DC.SAADAM_DEFINE_XML_PATH,
            DC.SAADAM_UNDERLYING_EXPRESSION,
            DC.SAADAM_AFFECTED_FULL_METHOD,
            DC.SAADAM_AFFECTED_METHOD_RETURN_TYPE,
            DC.SAADAM_AFFECTED_METHOD_HASH,
    }, "Spring AOP通知影响的方法，记录每个通知实际作用到的目标方法（被代理方法）"),
    DTIE_FIELD_ANNOTATION("field_annotation", new String[]{
            DC.FA_RECORD_ID,
            DC.FA_SIMPLE_CLASS_NAME,
            DC.FA_FIELD_NAME,
            DC.FA_ANNOTATION_NAME,
            DC.FA_ATTRIBUTE_NAME,
            DC.FA_ATTRIBUTE_TYPE,
            DC.FA_ATTRIBUTE_VALUE,
            DC.FA_CLASS_NAME,
    }, "字段注解信息，记录每个字段上的注解及属性、所属类"),
    DTIE_FIELD_INFO("field_info", DC.FIELD_INFO_COLUMNS,
            "字段信息，记录每个字段的名称、类型、所属类、access flags等"),
    DTIE_DUP_FIELD_INFO("dup_field_info", DC.FIELD_INFO_COLUMNS,
            "重复同名类的字段信息，记录包名不同但简单类名相同的类中的字段信息"),
    DTIE_FIELD_USAGE_OTHER("field_usage_other", new String[]{
            DC.FUO_RECORD_ID,
            DC.FUO_FULL_METHOD,
            DC.FUO_METHOD_RETURN_TYPE,
            DC.FUO_STATIC_FLAG,
            DC.FUO_GET_OR_PUT,
            DC.FUO_FIELD_IN_SIMPLE_CLASS_NAME,
            DC.FUO_FIELD_NAME,
            DC.FUO_FIELD_TYPE,
            DC.FUO_LINE_NUMBER,
            DC.FUO_SIMPLE_CLASS_NAME,
            DC.FUO_CLASS_NAME,
            DC.FUO_METHOD_HASH,
            DC.FUO_FIELD_IN_CLASS_NAME,
            DC.FUO_CLASS_JAR_NUM,
            DC.FUO_FIELD_JAR_NUM
    }, "使用其他类中字段的使用情况，记录方法中对其他类字段的get/put操作、字段类型、行号等"),
    DTIE_GET_METHOD("get_method", DC.GET_SET_METHOD_COLUMNS,
            "dto的get方法及字段，记录dto类中get方法对应的字段信息"),
    DTIE_SET_METHOD("set_method", DC.GET_SET_METHOD_COLUMNS,
            "dto的set方法及字段，记录dto类中set方法对应的字段信息"),
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
    }, "set方法赋值信息，记录set方法被调用时值的来源（方法调用链路、字段关联、等价转换等）"),
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
    }, "通过get/set方法关联的字段关系，记录同一字段被set写入与被get读取的调用关系，用于分析字段数据流向"),
    DTIE_MYBATIS_MS_COLUMN("mybatis_ms_column", new String[]{
            DC.MMC_RECORD_ID,
            DC.MMC_RESULT_MAP_ID,
            DC.MMC_ENTITY_SIMPLE_CLASS_NAME,
            DC.MMC_ENTITY_FIELD_NAME,
            DC.MMC_COLUMN_NAME,
            DC.MMC_COLUMN_TYPE,
            DC.MMC_ENTITY_CLASS_NAME,
            DC.MMC_XML_FILE_NAME,
            DC.MMC_XML_FILE_PATH,
    }, "MyBatis resultMap列映射，记录实体字段与数据库列的对应关系及列类型"),
    DTIE_MYBATIS_MS_ENTITY("mybatis_ms_entity", new String[]{
            DC.MME_RECORD_ID,
            DC.MME_MAPPER_SIMPLE_CLASS_NAME,
            DC.MME_ENTITY_SIMPLE_CLASS_NAME,
            DC.MME_TABLE_NAME,
            DC.MME_MAPPER_CLASS_NAME,
            DC.MME_ENTITY_CLASS_NAME,
            DC.MME_XML_FILE_NAME,
            DC.MME_XML_FILE_PATH,
    }, "MyBatis Mapper对应的实体与表，记录Mapper方法操作的实体类与数据库表"),
    DTIE_FIELD_GENERICS_TYPE("field_generics_type", new String[]{
            DC.FGT_RECORD_ID,
            DC.FGT_SIMPLE_CLASS_NAME,
            DC.FGT_FIELD_NAME,
            DC.FGT_TYPE,
            DC.FGT_TYPE_SEQ,
            DC.FGT_SIMPLE_GENERICS_TYPE_NAD,
            DC.FGT_GENERICS_ARRAY_DIMENSIONS,
            DC.FGT_TYPE_VARIABLES_NAME,
            DC.FGT_WILDCARD,
            DC.FGT_REFERENCE_TYPE,
            DC.FGT_GENERICS_CATEGORY,
            DC.FGT_GENERICS_TYPE_NAD,
            DC.FGT_CLASS_NAME,
    }, "非静态字段集合中涉及的泛型类型，记录字段泛型的类型、数组维度、通配符等"),
    DTIE_PROPERTIES_CONF("properties_conf", new String[]{
            DC.PC_RECORD_ID,
            DC.PC_PROPERTIES_KEY,
            DC.PC_PROPERTIES_FILE_PATH,
            DC.PC_PROPERTIES_FILE_NAME,
            DC.PC_PROPERTIES_VALUE,
    }, "properties配置文件信息，记录properties文件的key、value及来源文件"),
    DTIE_XML_CONF("xml_conf", new String[]{
            DC.XC_RECORD_ID,
            DC.XC_XML_FILE_PATH,
            DC.XC_XML_FILE_NAME,
            DC.XC_XML_FILE_SEQ,
            DC.XC_ELEMENT_SEQ,
            DC.XC_PARENT_SEQ,
            DC.XC_IN_ELEMENT_SEQ,
            DC.XC_TYPE,
            DC.XC_NESTED_ELEMENT_NAME,
            DC.XC_ELEMENT_NAME,
            DC.XC_ATTRIBUTE_NAME,
            DC.XC_ELEMENT_VALUE
    }, "xml配置文件信息，按层级记录xml文件中元素/属性的名称与值"),
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
    }, "MyBatis Mapper写入列，记录insert/update中写入的数据库列与参数对象/属性对应关系"),
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
    }, "MyBatis Mapper条件列，记录where条件中数据库列与参数对象/属性、操作符的对应关系"),
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
    }, "MyBatis Mapper查询列，记录select中数据库列与别名、实体字段的对应关系"),
    DTIE_METHOD_RETURN_ARG_SEQ("method_return_arg_seq", new String[]{
            DC.MRAS_RECORD_ID,
            DC.MRAS_METHOD_HASH,
            DC.MRAS_RETURN_ARG_SEQ,
            DC.MRAS_FULL_METHOD,
            DC.MRAS_RETURN_TYPE,
            DC.MRAS_EQUIVALENT_CONVERSION,
    }, "方法返回值对应的参数序号，记录方法直接返回某个参数时该参数的序号及等价转换"),
    DTIE_METHOD_RETURN_CALL_ID("method_return_call_id", new String[]{
            DC.MRCI_RECORD_ID,
            DC.MRCI_METHOD_HASH,
            DC.MRCI_RETURN_CALL_ID,
            DC.MRCI_FULL_METHOD,
            DC.MRCI_RETURN_TYPE,
            DC.MRCI_EQUIVALENT_CONVERSION,
    }, "方法返回值对应的方法调用ID，记录方法返回某个方法调用结果时该调用的ID及等价转换"),
    DTIE_METHOD_RETURN_CONST_VALUE("method_return_const_value", new String[]{
            DC.MRCV_RECORD_ID,
            DC.MRCV_METHOD_HASH,
            DC.MRCV_SEQ,
            DC.MRCV_CONST_TYPE,
            DC.MRCV_CONST_VALUE,
            DC.MRCV_FULL_METHOD,
            DC.MRCV_RETURN_TYPE,
    }, "方法返回的常量值（含null），记录方法返回常量时的类型与值"),
    DTIE_METHOD_RETURN_FIELD_INFO("method_return_field_info", new String[]{
            DC.MRFI_RECORD_ID,
            DC.MRFI_METHOD_HASH,
            DC.MRFI_SEQ,
            DC.MRFI_STATIC_FIELD,
            DC.MRFI_FIELD_OF_THIS,
            DC.MRFI_FIELD_IN_SIMPLE_CLASS_NAME,
            DC.MRFI_SIMPLE_FIELD_TYPE_NAD,
            DC.MRFI_FIELD_ARRAY_DIMENSIONS,
            DC.MRFI_FIELD_NAME,
            DC.MRFI_FIELD_IN_CLASS_NAME,
            DC.MRFI_FIELD_TYPE_NAD,
            DC.MRFI_FULL_METHOD,
            DC.MRFI_RETURN_TYPE,
    }, "方法返回的字段（含枚举），记录方法返回某个字段时字段的类型、名称、是否静态/this字段等"),
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
            DC.MCTH_RETURN_TYPE,
    }, "方法的catch信息，记录try-catch块的行号、offset、catch的异常类型及对应的方法调用ID范围"),
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
            DC.MF_RETURN_TYPE,
    }, "方法的finally信息，记录try-catch-finally块的行号、方法调用ID范围及finally起始行号"),
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
            DC.MT_RETURN_TYPE,
    }, "方法通过throw抛出的异常信息，记录throw的异常类型、行号、来源（catch异常/方法调用返回值/未知）等"),
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
    }, "MyBatis get/set方法与数据库操作的关系，记录字段get/set对应的数据库表、列及读写操作"),
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
            DC.MCMCR_CALLEE_RETURN_TYPE,
    }, "方法调用使用方法调用返回值，记录被调用对象或参数为另一方法调用返回值时的调用ID与被调用方法"),
    DTIE_MYBATIS_MS_FORMATED_SQL("mybatis_ms_formated_sql", new String[]{
            DC.MMFS_RECORD_ID,
            DC.MMFS_XML_FILE_NAME,
            DC.MMFS_SQL_ID,
            DC.MMFS_SQL_SEQ,
            DC.MMFS_XML_ELEMENT_NAME,
            DC.MMFS_FORMATED_SQL,
            DC.MMFS_SQL_HASH,
            DC.MMFS_MAPPER_SIMPLE_CLASS_NAME,
            DC.MMFS_MAPPER_CLASS_NAME,
            DC.MMFS_XML_FILE_PATH,
            DC.MMFS_RESULT_MAP_ID,
            DC.MMFS_RESULT_MAP_HASH,
    }, "MyBatis格式化后的SQL，记录XML中每条SQL的id、格式化后文本、hash及所属Mapper"),
    DTIE_PACKAGE_INFO("package_info", new String[]{
            DC.PI_RECORD_ID,
            DC.PI_PACKAGE_NAME,
            DC.PI_PACKAGE_LEVEL,
            DC.PI_JAR_NUM,
            DC.PI_JAR_FILE_NAME
    }, "包名信息，记录每个包的层级、所属jar序号与文件名"),
    ;

    private final String tableNameKeyword;

    private final String[] columns;

    // 数据库表额外描述，代表该表存储的数据及作用（用作代码图谱查询时给AI理解）
    private final String extraDesc;

    DbTableInfoEnum(String tableNameKeyword, String[] columns, String extraDesc) {
        this.tableNameKeyword = tableNameKeyword;
        this.columns = columns;
        this.extraDesc = extraDesc;
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
     * 获取表名模板，固定使用前缀"jacg_"，在表名后拼接 {appName} 占位符（不含下划线，与SQL文件约定一致）。
     * 返回值形如 jacg_config{appName}，后续由 JACGSqlUtil.replaceFlagInSql 在非空时补下划线、空时不补。
     *
     * @return 表名模板（含 {appName} 占位符）
     */
    public String getTableName() {
        // 模板不预置下划线，下划线由 replaceFlagInSql 按后缀是否为空决定是否补，避免空后缀残留 "_"
        return JACGConstants.TABLE_PREFIX + tableNameKeyword + JACGConstants.REPLACE_SQL_FLAG_APP_NAME;
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

    /**
     * 构造真实/期望表名（公共方法，供 getTableName/isExpectedTableName/getDbTableInfoEnumByTableName 共用，杜绝不一致）。
     * flag 为空 → 固定表名 jacg_{keyword}（无后缀）；flag 以 "_" 开头 → 不重复加分隔符（jar diff 归一，避免双下划线）。
     *
     * @param keyword 表名关键字
     * @param flag    appName + tableSuffix 组成的后缀，可能为空或以 "_" 开头
     * @return 表名
     */
    public static String buildTableName(String keyword, String flag) {
        if (flag == null || flag.isEmpty()) {
            // 后缀为空：固定表名，无尾部下划线
            return JACGConstants.TABLE_PREFIX + keyword;
        }
        // flag 以 "_" 开头时（jar diff 的 _1/_2 配空 appName）不重复加分隔符，避免双下划线
        String sep = flag.startsWith("_") ? "" : JACGConstants.FLAG_UNDER_LINE;
        return JACGConstants.TABLE_PREFIX + keyword + sep + flag;
    }

    private String getTableName(String flag) {
        return buildTableName(tableNameKeyword, flag);
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

    /**
     * 获取数据库表额外描述，代表该表存储的数据及作用
     *
     * @return
     */
    public String getExtraDesc() {
        return extraDesc;
    }
}
