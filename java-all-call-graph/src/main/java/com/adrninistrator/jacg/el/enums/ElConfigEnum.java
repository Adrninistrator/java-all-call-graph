package com.adrninistrator.jacg.el.enums;

import com.adrninistrator.jacg.common.enums.InputDirEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.el.checker.ElChecker4JCCClassReference;
import com.adrninistrator.jacg.el.checker.ElChecker4JarDiff;
import com.adrninistrator.jacg.el.checker.ElChecker4MethodCall;
import com.adrninistrator.jacg.el.checker.ElChecker4SpringAOP;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.el.checker.AbstractElChecker;
import com.adrninistrator.javacg2.el.enums.CommonElAllowedVariableEnum;
import com.adrninistrator.javacg2.el.enums.JavaCG2ElAllowedVariableEnum;
import com.adrninistrator.javacg2.el.enums.interfaces.ElAllowedVariableInterface;
import com.adrninistrator.javacg2.el.enums.interfaces.ElConfigInterface;

/**
 * @author adrninistrator
 * @date 2025/2/15
 * @description: 使用表达式语言的配置文件枚举
 */
public enum ElConfigEnum implements ElConfigInterface {
    ECE_EXAMPLE(InputDirEnum.IDE_EL_EXAMPLE.getDirName() + "/el_usage" + JavaCG2Constants.EXT_MD, new String[]{"表达式示例文件"}, null, null, false),
    ECE_GEN_ALL_CALL_GRAPH_IGNORE_METHOD_CALL(InputDirEnum.IDE_GEN_ALL_CALL_GRAPH.getDirName() + "/gen_call_graph_ignore_method_call.av",
            new String[]{"指定生成方法完整调用链时是否跳过解析特定的方法调用，支持通过方法调用类型、调用方法或被调用方法等判断"},
            new ElAllowedVariableInterface[]{
                    CommonElAllowedVariableEnum.EAVE_METHOD_CALL_TYPE,
                    CommonElAllowedVariableEnum.EAVE_MC_ER_CLASS_NAME,
                    CommonElAllowedVariableEnum.EAVE_MC_ER_PACKAGE_NAME,
                    CommonElAllowedVariableEnum.EAVE_MC_ER_SIMPLE_CLASS_NAME,
                    CommonElAllowedVariableEnum.EAVE_MC_ER_METHOD_NAME,
                    CommonElAllowedVariableEnum.EAVE_MC_ER_METHOD_ARG_NUM,
                    CommonElAllowedVariableEnum.EAVE_MC_ER_FULL_METHOD,
                    CommonElAllowedVariableEnum.EAVE_MC_EE_CLASS_NAME,
                    CommonElAllowedVariableEnum.EAVE_MC_EE_PACKAGE_NAME,
                    CommonElAllowedVariableEnum.EAVE_MC_EE_SIMPLE_CLASS_NAME,
                    CommonElAllowedVariableEnum.EAVE_MC_EE_METHOD_NAME,
                    CommonElAllowedVariableEnum.EAVE_MC_EE_METHOD_ARG_NUM,
                    CommonElAllowedVariableEnum.EAVE_MC_EE_FULL_METHOD,
                    ElAllowedVariableEnum.EAVE_MC_FLAGS_ENUM
            }, ElChecker4MethodCall.class, true),
    ECE_SPRING_AOP_IGNORE_SPRING_BEAN_CLASS(InputDirEnum.IDE_SPRING_AOP.getDirName() + "/spring_aop_ignore_spring_bean_class.av",
            new String[]{"指定解析Spring AOP影响方法时忽略哪些Spring Bean类，支持指定类名、包名、简单类名"},
            new ElAllowedVariableInterface[]{
                    JavaCG2ElAllowedVariableEnum.EAVE_PARSE_CLASS_NAME,
                    JavaCG2ElAllowedVariableEnum.EAVE_PARSE_PACKAGE_NAME,
                    JavaCG2ElAllowedVariableEnum.EAVE_PARSE_SIMPLE_CLASS_NAME,
            }, ElChecker4SpringAOP.class, true),
    ECE_JAR_DIFF_GEN_ALL_CALL_GRAPH_IGNORE_CALLEE(InputDirEnum.IDE_JAR_DIFF.getDirName() + "/jar_diff_gen_all_call_graph_ignore_callee.av",
            new String[]{"JarDiff获得发生变化的方法的影响范围时（生成向上的方法完整调用链及调用堆栈），指定发生变化的方法中，需要忽略的方法"},
            new ElAllowedVariableInterface[]{
                    JavaCG2ElAllowedVariableEnum.EAVE_PARSE_CLASS_NAME,
                    JavaCG2ElAllowedVariableEnum.EAVE_PARSE_PACKAGE_NAME,
                    JavaCG2ElAllowedVariableEnum.EAVE_PARSE_SIMPLE_CLASS_NAME,
                    JavaCG2ElAllowedVariableEnum.EAVE_PARSE_METHOD_NAME,
                    JavaCG2ElAllowedVariableEnum.EAVE_PARSE_METHOD_ARG_NUM,
                    JavaCG2ElAllowedVariableEnum.EAVE_PARSE_FULL_METHOD,
            }, ElChecker4JarDiff.class, true),
    ECE_JAR_DIFF_GEN_ALL_CALL_GRAPH_IGNORE_CALLER(InputDirEnum.IDE_JAR_DIFF.getDirName() + "/jar_diff_gen_all_call_graph_ignore_caller.av",
            new String[]{"JarDiff获得发生变化的方法向下的方法完整调用链时，指定发生变化的方法中，需要忽略的方法"},
            new ElAllowedVariableInterface[]{
                    JavaCG2ElAllowedVariableEnum.EAVE_PARSE_CLASS_NAME,
                    JavaCG2ElAllowedVariableEnum.EAVE_PARSE_PACKAGE_NAME,
                    JavaCG2ElAllowedVariableEnum.EAVE_PARSE_SIMPLE_CLASS_NAME,
                    JavaCG2ElAllowedVariableEnum.EAVE_PARSE_METHOD_NAME,
                    JavaCG2ElAllowedVariableEnum.EAVE_PARSE_METHOD_ARG_NUM,
                    JavaCG2ElAllowedVariableEnum.EAVE_PARSE_FULL_METHOD,
            }, ElChecker4JarDiff.class, true),
    ECE_COMPATIBILITY_CHECK_IGNORE_CLASS_REFERENCE(InputDirEnum.IDE_COMPATIBILITY.getDirName() + "/compatibility_check_ignore_class_reference.av",
            new String[]{"指定Jar兼容性检查快速模式时是否跳过记录特定的类引用关系"},
            new ElAllowedVariableInterface[]{
                    CommonElAllowedVariableEnum.EAVE_MC_ER_CLASS_NAME,
                    CommonElAllowedVariableEnum.EAVE_MC_ER_PACKAGE_NAME,
                    CommonElAllowedVariableEnum.EAVE_MC_ER_SIMPLE_CLASS_NAME,
                    CommonElAllowedVariableEnum.EAVE_MC_EE_CLASS_NAME,
                    CommonElAllowedVariableEnum.EAVE_MC_EE_PACKAGE_NAME,
                    CommonElAllowedVariableEnum.EAVE_MC_EE_SIMPLE_CLASS_NAME
            }, ElChecker4JCCClassReference.class, true),
    ;

    // 配置文件名称
    private final String fileName;

    // 配置文件描述
    private final String[] descriptions;

    // 配置文件允许使用的表达式变量枚举
    private final ElAllowedVariableInterface[] elAllowedVariableEnums;

    // 用于提前执行表达式进行检查的类
    private final Class<? extends AbstractElChecker> elCheckClass;

    // 当前参数是否用于跳过数据
    private final boolean ignoreData;

    ElConfigEnum(String fileName, String[] descriptions, ElAllowedVariableInterface[] elAllowedVariableEnums, Class<? extends AbstractElChecker> elCheckClass,
                 boolean ignoreData) {
        this.fileName = fileName;
        this.descriptions = descriptions;
        this.elAllowedVariableEnums = elAllowedVariableEnums;
        this.elCheckClass = elCheckClass;
        this.ignoreData = ignoreData;
    }

    @Override
    public String getEnumConstantName() {
        return name();
    }

    @Override
    public String getKey() {
        return fileName;
    }

    @Override
    public String[] getDescriptions() {
        return descriptions;
    }

    @Override
    public Class<? extends AbstractElChecker> getElCheckClass() {
        return elCheckClass;
    }

    @Override
    public boolean isIgnoreData() {
        return ignoreData;
    }

    @Override
    public String getConfigPrintInfo() {
        return fileName + " " + ElConfigEnum.class.getSimpleName() + "." + name();
    }

    @Override
    public String genConfigUsage() {
        return doGenConfigUsage(ConfigureWrapper.class);
    }

    @Override
    public ElAllowedVariableInterface[] getElAllowedVariableEnums() {
        return elAllowedVariableEnums;
    }
}
