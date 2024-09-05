package com.adrninistrator.jacg.runner;

import com.adrninistrator.jacg.annotation.attributes.AnnotationAttributesFormatter;
import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.extensions.codeparser.jarentryotherfile.MyBatisMySqlColumnInfoCodeParser;
import com.adrninistrator.jacg.extensions.codeparser.jarentryotherfile.MyBatisMySqlEntityInfoCodeParser;
import com.adrninistrator.jacg.extensions.codeparser.jarentryotherfile.MyBatisMySqlSelectColumnCodeParser;
import com.adrninistrator.jacg.extensions.codeparser.jarentryotherfile.MyBatisMySqlSetColumnCodeParser;
import com.adrninistrator.jacg.extensions.codeparser.jarentryotherfile.MyBatisMySqlSqlInfoCodeParser;
import com.adrninistrator.jacg.extensions.codeparser.jarentryotherfile.MyBatisMySqlWhereColumnCodeParser;
import com.adrninistrator.jacg.extensions.codeparser.jarentryotherfile.MyBatisMySqlWriteSqlInfoCodeParser;
import com.adrninistrator.jacg.extensions.codeparser.jarentryotherfile.PropertiesConfCodeParser;
import com.adrninistrator.jacg.extensions.codeparser.jarentryotherfile.SpringTaskXmlCodeParser;
import com.adrninistrator.jacg.extensions.codeparser.jarentryotherfile.SpringXmlBeanParser;
import com.adrninistrator.jacg.extensions.codeparser.methodannotation.MyBatisAnnotationCodeParser;
import com.adrninistrator.jacg.markdown.writer.MarkdownWriter;
import com.adrninistrator.jacg.runner.base.AbstractRunner;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.common.enums.JavaCG2ConfigKeyEnum;
import com.adrninistrator.javacg2.common.enums.JavaCG2OtherConfigFileUseListEnum;
import com.adrninistrator.javacg2.common.enums.JavaCG2OtherConfigFileUseSetEnum;
import com.adrninistrator.javacg2.conf.JavaCG2ConfigureWrapper;
import com.adrninistrator.javacg2.dto.output.JavaCG2OutputInfo;
import com.adrninistrator.javacg2.entry.JavaCG2Entry;
import com.adrninistrator.javacg2.extensions.codeparser.CodeParserInterface;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2023/4/2
 * @description: 生成Java方法调用关系文件
 */
public class RunnerWriteCallGraphFile extends AbstractRunner {
    private static final Logger logger = LoggerFactory.getLogger(RunnerWriteCallGraphFile.class);

    // java-callgraph2输出文件信息
    protected JavaCG2OutputInfo javaCG2OutputInfo;

    // java-callgraph2的配置包装类
    protected JavaCG2ConfigureWrapper javaCG2ConfigureWrapper;

    // java-callgraph2的入口类
    private JavaCG2Entry javaCG2Entry;

    public RunnerWriteCallGraphFile() {
        super();
    }

    public RunnerWriteCallGraphFile(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
    }

    /**
     * 入口方法
     * 可以调用run(ConfigureWrapper configureWrapper)方法，不需要指定JavaCG2ConfigureWrapper参数
     *
     * @param javaCG2ConfigureWrapper
     * @return
     */
    public boolean run(JavaCG2ConfigureWrapper javaCG2ConfigureWrapper) {
        this.javaCG2ConfigureWrapper = javaCG2ConfigureWrapper;
        return run();
    }

    @Override
    protected boolean preHandle() {
        return true;
    }

    @Override
    protected void handle() {
        // 调用java-callgraph2生成jar包的方法调用关系
        if (!callJavaCallGraph2(null)) {
            // 记录执行失败
            recordTaskFail();
        }

        // 打印重复的类名
        printDuplicateClasses();
    }

    @Override
    protected boolean checkH2DbFile() {
        // 不检查H2数据库文件
        return true;
    }

    // 调用java-callgraph2生成jar包的方法调用关系
    protected boolean callJavaCallGraph2(List<String> jarPathList) {
        List<String> usedJarPathList;
        if (!JavaCG2Util.isCollectionEmpty(jarPathList)) {
            usedJarPathList = jarPathList;
        } else {
            // 获得需要处理的jar包列表
            usedJarPathList = getJarPathList();
        }
        if (JavaCG2Util.isCollectionEmpty(usedJarPathList)) {
            logger.error("请在配置文件 {} 中指定需要处理的jar包，或保存class、jar文件的目录", OtherConfigFileUseListEnum.OCFULE_JAR_DIR.getConfigPrintInfo());
            return false;
        }

        for (String jarPath : usedJarPathList) {
            if (!new File(jarPath).exists()) {
                logger.error("文件或目录不存在 {}", jarPath);
                return false;
            }
        }

        // 生成java-callgraph2使用的配置信息
        if (javaCG2ConfigureWrapper == null) {
            javaCG2ConfigureWrapper = configureWrapper.genJavaCG2ConfigureWrapper();
        }
        javaCG2Entry = new JavaCG2Entry(javaCG2ConfigureWrapper);

        // 设置对注解属性进行格式化的类
        javaCG2Entry.setAnnotationAttributesFormatter(new AnnotationAttributesFormatter());

        // 添加用于对代码进行解析的处理类
        if (!addCodeParserExtensions()) {
            return false;
        }

        // 调用java-callgraph2
        logger.info("调用java-callgraph2生成jar包的方法调用关系");
        boolean success = javaCG2Entry.run();
        if (!success) {
            logger.error("调用java-callgraph2生成jar包的方法调用关系失败");
            return false;
        }

        // 获取输出信息
        javaCG2OutputInfo = javaCG2Entry.getJavaCG2OutputInfo();
        currentOutputDirPath = javaCG2OutputInfo.getOutputDirPath();

        // 打印当前使用的配置信息
        printAllConfigInfo();

        // 打印java-callgraph2当前使用的配置信息
        printJavaCG2UsedConfigInfo();
        return true;
    }

    // 添加代码解析扩展类
    private boolean addCodeParserExtensions() {
        List<String> codeParserExtensionClassList = configureWrapper.getOtherConfigList(OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_CODE_PARSER, true);
        Set<String> codeParserExtensionClassSet = new HashSet<>(codeParserExtensionClassList);
        if (codeParserExtensionClassList.size() != codeParserExtensionClassSet.size()) {
            logger.info("指定的用于对代码进行解析的扩展类存在重复 {} {}", OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_CODE_PARSER.getConfigPrintInfo(),
                    StringUtils.join(codeParserExtensionClassList));
            return false;
        }
        if (codeParserExtensionClassSet.contains(MyBatisMySqlSqlInfoCodeParser.class.getName())) {
            logger.info("用于对代码进行解析的扩展类不能在配置文件 {} 中指定 {}", OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_CODE_PARSER.getConfigPrintInfo(),
                    MyBatisMySqlSqlInfoCodeParser.class.getName());
            return false;
        }

        // 添加默认的代码解析扩展类
        MyBatisMySqlSqlInfoCodeParser myBatisMySqlSqlInfoCodeParser = new MyBatisMySqlSqlInfoCodeParser();

        MyBatisMySqlColumnInfoCodeParser myBatisMySqlColumnInfoCodeParser = new MyBatisMySqlColumnInfoCodeParser();
        MyBatisMySqlEntityInfoCodeParser myBatisMySqlEntityInfoCodeParser = new MyBatisMySqlEntityInfoCodeParser();
        MyBatisMySqlWriteSqlInfoCodeParser myBatisMySqlWriteSqlInfoCodeParser = new MyBatisMySqlWriteSqlInfoCodeParser();
        MyBatisMySqlSetColumnCodeParser myBatisMySqlSetColumnCodeParser = new MyBatisMySqlSetColumnCodeParser();
        MyBatisMySqlSelectColumnCodeParser myBatisMySqlSelectColumnCodeParser = new MyBatisMySqlSelectColumnCodeParser();
        MyBatisMySqlWhereColumnCodeParser myBatisMySqlWhereColumnCodeParser = new MyBatisMySqlWhereColumnCodeParser();

        myBatisMySqlSqlInfoCodeParser.setMyBatisMySqlColumnInfoCodeParser(myBatisMySqlColumnInfoCodeParser);
        myBatisMySqlSqlInfoCodeParser.setMyBatisMySqlEntityInfoCodeParser(myBatisMySqlEntityInfoCodeParser);
        myBatisMySqlSqlInfoCodeParser.setMyBatisMySqlWriteSqlInfoCodeParser(myBatisMySqlWriteSqlInfoCodeParser);
        myBatisMySqlSqlInfoCodeParser.setMyBatisMySqlSetColumnCodeParser(myBatisMySqlSetColumnCodeParser);
        myBatisMySqlSqlInfoCodeParser.setMyBatisMySqlSelectColumnCodeParser(myBatisMySqlSelectColumnCodeParser);
        myBatisMySqlSqlInfoCodeParser.setMyBatisMySqlWhereColumnCodeParser(myBatisMySqlWhereColumnCodeParser);

        javaCG2Entry.addCodeParser(myBatisMySqlSqlInfoCodeParser);
        javaCG2Entry.addCodeParser(myBatisMySqlColumnInfoCodeParser);
        javaCG2Entry.addCodeParser(myBatisMySqlEntityInfoCodeParser);
        javaCG2Entry.addCodeParser(myBatisMySqlWriteSqlInfoCodeParser);
        javaCG2Entry.addCodeParser(myBatisMySqlSetColumnCodeParser);
        javaCG2Entry.addCodeParser(myBatisMySqlSelectColumnCodeParser);
        javaCG2Entry.addCodeParser(myBatisMySqlWhereColumnCodeParser);
        javaCG2Entry.addCodeParser(new SpringTaskXmlCodeParser());
        javaCG2Entry.addCodeParser(new MyBatisAnnotationCodeParser());
        javaCG2Entry.addCodeParser(new PropertiesConfCodeParser());
        javaCG2Entry.addCodeParser(new SpringXmlBeanParser());

        // 添加参数配置中的代码解析扩展类
        if (!JavaCG2Util.isCollectionEmpty(codeParserExtensionClassList)) {
            logger.info("添加参数配置中的代码解析扩展类\n{}", StringUtils.join(codeParserExtensionClassList, "\n"));
            for (String extensionClass : codeParserExtensionClassList) {
                CodeParserInterface codeParser = JACGUtil.genClassObject(extensionClass, CodeParserInterface.class);
                if (codeParser == null) {
                    return false;
                }
                // 添加代码解析扩展类
                javaCG2Entry.addCodeParser(codeParser);
            }
        }
        return true;
    }

    /**
     * 打印当前使用的配置信息
     */
    protected void printJavaCG2UsedConfigInfo() {
        String configMdFilePath = JavaCG2Util.addSeparator4FilePath(currentOutputDirPath) + JACGConstants.FILE_JAVACG2_ALL_CONFIG_MD;
        logger.info("{} 全部的配置参数信息保存到以下文件 {}", JavaCG2Entry.class.getSimpleName(), configMdFilePath);
        try (MarkdownWriter markdownWriter = new MarkdownWriter(configMdFilePath, true)) {
            // 打印java-callgraph2的主要配置信息
            printJavaCG2MainConfigInfo(markdownWriter);

            // 打印Set格式的其他配置信息
            printJavaCG2OtherSetConfigInfo(markdownWriter);

            // 打印List格式的其他配置信息
            printJavaCG2OtherListConfigInfo(markdownWriter);
        } catch (Exception e) {
            logger.error("{} error ", currentSimpleClassName, e);
        }
    }

    // 打印java-callgraph2的主要配置信息
    private void printJavaCG2MainConfigInfo(MarkdownWriter markdownWriter) throws IOException {
        // 添加公共的说明
        configureWrapper.addCommonDesc(markdownWriter);
        markdownWriter.addTitle(1, JACGConstants.MAIN_CONFIG);
        // 写入配置文件名
        markdownWriter.addTitle(2, JavaCG2Constants.DIR_CONFIG + "/" + JavaCG2Constants.FILE_CONFIG);
        markdownWriter.addListWithNewLine(JACGConstants.USED_CONFIG_FLAG_FILE_ENUM_CLASS);
        markdownWriter.addLineWithNewLine(JavaCG2ConfigKeyEnum.class.getSimpleName());
        markdownWriter.addTableHead(JACGConstants.USED_CONFIG_FLAG_CONF_KEY, JACGConstants.USED_CONFIG_FLAG_CONF_ENUM_NAME, JACGConstants.USED_CONFIG_FLAG_CONF_DESC,
                JACGConstants.USED_CONFIG_FLAG_CONF_VALUE);
        for (JavaCG2ConfigKeyEnum javaCG2ConfigKeyEnum : JavaCG2ConfigKeyEnum.values()) {
            String value = javaCG2ConfigureWrapper.getConfig(null, javaCG2ConfigKeyEnum, false);
            markdownWriter.addTableBody(javaCG2ConfigKeyEnum.getKey(), javaCG2ConfigKeyEnum.name(), javaCG2ConfigKeyEnum.getDesc(), (value == null ? "" : value));
        }
        markdownWriter.addEmptyLine();
    }

    // 打印List格式的其他配置信息
    private void printJavaCG2OtherListConfigInfo(MarkdownWriter markdownWriter) throws IOException {
        JavaCG2OtherConfigFileUseListEnum[] configs = JavaCG2OtherConfigFileUseListEnum.values();
        for (int i = 0; i < configs.length; i++) {
            JavaCG2OtherConfigFileUseListEnum currentConfig = configs[i];
            if (JavaCG2OtherConfigFileUseListEnum.OCFULE_CODE_PARSER_ONLY_4SHOW == currentConfig) {
                // 代码解析扩展类名特殊处理
                List<String> allCodeParserNameList = javaCG2Entry.getAllCodeParserNameList();
                configureWrapper.doPrintListConfigInfo(markdownWriter, i, currentConfig.getFileName(), currentConfig.getClass().getSimpleName(), currentConfig.name(),
                        currentConfig.getDesc(), allCodeParserNameList);
                continue;
            }
            configureWrapper.doPrintListConfigInfo(markdownWriter, i, currentConfig.getFileName(), currentConfig.getClass().getSimpleName(), currentConfig.name(),
                    currentConfig.getDesc(), javaCG2ConfigureWrapper.getOtherConfigList(currentConfig, false));
        }
    }

    // 打印Set格式的其他配置信息
    private void printJavaCG2OtherSetConfigInfo(MarkdownWriter markdownWriter) throws IOException {
        JavaCG2OtherConfigFileUseSetEnum[] configs = JavaCG2OtherConfigFileUseSetEnum.values();
        for (int i = 0; i < configs.length; i++) {
            JavaCG2OtherConfigFileUseSetEnum currentConfig = configs[i];
            configureWrapper.doPrintSetConfigInfo(markdownWriter, i, currentConfig.getFileName(), currentConfig.getClass().getSimpleName(), currentConfig.name(),
                    currentConfig.getDesc(), javaCG2ConfigureWrapper.getOtherConfigSet(currentConfig, false));
        }
    }

    // 打印重复的类名
    protected void printDuplicateClasses() {
        Map<String, List<String>> duplicateClassNameMap = javaCG2Entry.getDuplicateClassNameMap();
        if (duplicateClassNameMap.isEmpty()) {
            logger.info("不存在重复的类名");
            return;
        }

        List<String> duplicateClassNameList = new ArrayList<>(duplicateClassNameMap.keySet());
        Collections.sort(duplicateClassNameList);

        for (String duplicateClassName : duplicateClassNameList) {
            List<String> classFilePathList = duplicateClassNameMap.get(duplicateClassName);
            logger.info("重复的类名 {} 使用的class文件 {}", duplicateClassName, classFilePathList.get(0));
            for (int i = 1; i < classFilePathList.size(); i++) {
                logger.info("重复的类名 {} 跳过的class文件 {}", duplicateClassName, classFilePathList.get(i));
            }
        }
    }

    @Override
    protected boolean handleDb() {
        // 返回不需要操作数据库
        return false;
    }
}
