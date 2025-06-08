package com.adrninistrator.jacg.conf.enums;

import com.adrninistrator.jacg.annotation.formatter.AbstractAnnotationFormatter;
import com.adrninistrator.jacg.annotation.formatter.DefaultAnnotationFormatter;
import com.adrninistrator.jacg.annotation.formatter.HideAnnotationFormatter;
import com.adrninistrator.jacg.annotation.formatter.SpringMvcRequestMappingFormatter;
import com.adrninistrator.jacg.annotation.formatter.SpringTransactionalFormatter;
import com.adrninistrator.jacg.common.enums.InputDirEnum;
import com.adrninistrator.jacg.extensions.findstackfilter.FindStackKeywordFilterInterface;
import com.adrninistrator.jacg.extensions.manualaddmethodcall.AbstractManualAddMethodCall1;
import com.adrninistrator.jacg.extensions.methodcall.AbstractJACGMethodCallExtension;
import com.adrninistrator.javacg2.conf.enums.interfaces.OtherConfigInterface;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;
import com.adrninistrator.javacg2.extensions.codeparser.CodeParserInterface;
import com.adrninistrator.javacg2.extensions.methodcall.JavaCG2MethodCallExtensionInterface;

/**
 * @author adrninistrator
 * @date 2022/4/20
 * @description:
 */
public enum OtherConfigFileUseListEnum implements OtherConfigInterface {
    OCFULE_FIND_STACK_KEYWORD_4EE(InputDirEnum.IDE_KEYWORD_CONF.getDirName() + "/find_stack_keyword_4ee.properties",
            new String[]{"生成向上的方法完整调用链文件后，再查找到起始方法的调用堆栈时，使用的关键字（每行指定一项配置，可指定多行）"},
            null),
    OCFULE_FIND_STACK_KEYWORD_4ER(InputDirEnum.IDE_KEYWORD_CONF.getDirName() + "/find_stack_keyword_4er.properties",
            new String[]{"生成向下的方法完整调用链文件后，再查找从起始方法开始的调用堆栈时，使用的关键字（每行指定一项配置，可指定多行）"}
            , null),
    OCFULE_EXTENSIONS_CODE_PARSER(InputDirEnum.IDE_EXTENSIONS.getDirName() + "/code_parser.properties",
            new String[]{"在此定义用于对代码进行解析的扩展类完整类名（每行指定一项配置，可指定多行）",
                    "需要是 " + CodeParserInterface.class.getName() + " 接口的实现类"}
            , null),
    OCFULE_EXTENSIONS_METHOD_ANNOTATION_FORMATTER(InputDirEnum.IDE_EXTENSIONS.getDirName() + "/method_annotation_formatter.properties",
            new String[]{"在此定义处理方法上的注解生成用于显示信息的扩展类完整类名（每行指定一项配置，可指定多行）",
                    "需要是 " + AbstractAnnotationFormatter.class.getName() + " 类的子类",
                    "假如需要显示方法上的注解，请将默认的方法注解处理类 " + DefaultAnnotationFormatter.class.getSimpleName() + " 在最后指定",
                    "假如不需要显示方法上的注解，请只指定不显示方法注解的处理类 " + HideAnnotationFormatter.class.getSimpleName(),
                    HideAnnotationFormatter.class.getName()}
            , new String[]{
            SpringMvcRequestMappingFormatter.class.getName(),
            SpringTransactionalFormatter.class.getName(),
            DefaultAnnotationFormatter.class.getName(),
    }),
    OCFULE_EXTENSIONS_MANUAL_ADD_METHOD_CALL1(InputDirEnum.IDE_EXTENSIONS.getDirName() + "/manual_add_method_call1.properties",
            new String[]{"在此定义人工添加方法调用关系的扩展类完整类名，处理特定的子类与实现类（每行指定一项配置，可指定多行）",
                    "需要是 " + AbstractManualAddMethodCall1.class.getName() + " 类的子类"}
            , null),
    OCFULE_EXTENSIONS_FIND_STACK_KEYWORD_FILTER(InputDirEnum.IDE_EXTENSIONS.getDirName() + "/find_stack_keyword_filter.properties",
            new String[]{"在此定义用于对完整调用链文件生成调用堆栈时使用的过滤器扩展类完整类名，若未指定则使用配置参数中的关键字（每行指定一项配置，可指定多行）",
                    "需要是 " + FindStackKeywordFilterInterface.class.getName() + " 接口的实现类"}
            , null),
    OCFULE_EXTENSIONS_JAVACG2_METHOD_CALL(InputDirEnum.IDE_EXTENSIONS.getDirName() + "/javacg2_method_call_extensions.properties",
            new String[]{"java-callgraph2 组件在处理方法调用时的扩展类（每行指定一项配置，可指定多行）",
                    "需要是 " + JavaCG2MethodCallExtensionInterface.class.getName() + " 接口的实现类"}
            , null),
    OCFULE_EXTENSIONS_JACG_METHOD_CALL(InputDirEnum.IDE_EXTENSIONS.getDirName() + "/jacg_method_call_extensions.properties",
            new String[]{"java-all-call-graph 组件在处理方法调用时的扩展类（每行指定一项配置，可指定多行）",
                    "需要是 " + AbstractJACGMethodCallExtension.class.getName() + " 类的子类"}
            , null),
    OCFULE_JAR_DIFF_CALLEE_GRAPH_DIR(InputDirEnum.IDE_JAR_DIFF.getDirName() + "/jar_diff_callee_graph_dir.properties",
            new String[]{"(作用) 指定新旧两个目录，比较其中的不同版本jar文件的方法修改情况，获得发生变化的方法的影响范围（生成向上的完整方法调用链及调用堆栈）",
                    "(内容) 第1行指定旧目录路径，第2行指定新目录路径",
                    "(示例) build/jar-diff-version-1",
                    "(示例) build/jar-diff-version-2",
                    "(示例) D:/test/build/jar-diff-version-1",
                    "(示例) D:/test/build/jar-diff-version-2"}
            , null),
    OCFULE_JAR_DIFF_CALLER_GRAPH_DIR(InputDirEnum.IDE_JAR_DIFF.getDirName() + "/jar_diff_caller_graph_dir.properties",
            new String[]{"(作用) 指定新旧两个目录，比较其中的不同版本jar文件的方法修改情况，向下的完整方法调用链",
                    "(内容) 第1行指定旧目录路径，第2行指定新目录路径",
                    "(示例) build/jar-diff-version-1",
                    "(示例) build/jar-diff-version-2",
                    "(示例) D:/test/build/jar-diff-version-1",
                    "(示例) D:/test/build/jar-diff-version-2"}
            , null),
    ;

    // 参数配置文件名
    private final String fileName;
    // 参数配置描述
    private final String[] descriptions;
    // 默认值
    private final String[] defaultValues;

    OtherConfigFileUseListEnum(String fileName, String[] descriptions, String[] defaultValues) {
        this.fileName = fileName;
        this.descriptions = descriptions;
        this.defaultValues = defaultValues;
    }

    @Override
    public String getEnumConstantsName() {
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
    public String[] getDefaultValues() {
        return defaultValues;
    }

    @Override
    public String getConfigPrintInfo() {
        return fileName + " " + OtherConfigFileUseListEnum.class.getSimpleName() + "." + name();
    }

    @Override
    public boolean isSetOrList() {
        return false;
    }

    @Override
    public OtherConfigFileUseListEnum getFromKey(String key) {
        for (OtherConfigFileUseListEnum otherConfigFileUseListEnum : OtherConfigFileUseListEnum.values()) {
            if (otherConfigFileUseListEnum.getKey().equals(key)) {
                return otherConfigFileUseListEnum;
            }
        }
        throw new JavaCG2RuntimeException("不存在的key " + key);
    }

    @Override
    public String toString() {
        return fileName;
    }
}
