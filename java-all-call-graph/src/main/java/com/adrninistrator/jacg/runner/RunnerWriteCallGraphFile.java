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
import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.common.enums.JavaCGConfigKeyEnum;
import com.adrninistrator.javacg.common.enums.JavaCGOtherConfigFileUseListEnum;
import com.adrninistrator.javacg.common.enums.JavaCGOtherConfigFileUseSetEnum;
import com.adrninistrator.javacg.conf.JavaCGConfigureWrapper;
import com.adrninistrator.javacg.dto.output.JavaCGOutputInfo;
import com.adrninistrator.javacg.extensions.codeparser.CodeParserInterface;
import com.adrninistrator.javacg.stat.JCallGraph;
import com.adrninistrator.javacg.util.JavaCGUtil;
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
    protected JavaCGOutputInfo javaCGOutputInfo;

    // java-callgraph2的配置包装类
    protected JavaCGConfigureWrapper javaCGConfigureWrapper;

    // java-callgraph2的入口类
    private JCallGraph jCallGraph;

    public RunnerWriteCallGraphFile() {
        super();
    }

    public RunnerWriteCallGraphFile(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
    }

    /**
     * 入口方法
     * 可以调用run(ConfigureWrapper configureWrapper)方法，不需要指定JavaCGConfigureWrapper参数
     *
     * @param javaCGConfigureWrapper
     * @return
     */
    public boolean run(JavaCGConfigureWrapper javaCGConfigureWrapper) {
        this.javaCGConfigureWrapper = javaCGConfigureWrapper;
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
        if (!JavaCGUtil.isCollectionEmpty(jarPathList)) {
            usedJarPathList = jarPathList;
        } else {
            // 获得需要处理的jar包列表
            usedJarPathList = getJarPathList();
        }
        if (JavaCGUtil.isCollectionEmpty(usedJarPathList)) {
            logger.error("请在配置文件 {} 中指定需要处理的jar包，或保存class、jar文件的目录", OtherConfigFileUseListEnum.OCFULE_JAR_DIR.getKey());
            return false;
        }

        for (String jarPath : usedJarPathList) {
            if (!new File(jarPath).exists()) {
                logger.error("文件或目录不存在 {}", jarPath);
                return false;
            }
        }

        // 生成java-callgraph2使用的配置信息
        if (javaCGConfigureWrapper == null) {
            javaCGConfigureWrapper = configureWrapper.genJavaCGConfigureWrapper();
        }

        jCallGraph = new JCallGraph();
        // 设置对注解属性进行格式化的类
        jCallGraph.setAnnotationAttributesFormatter(new AnnotationAttributesFormatter());

        // 添加用于对代码进行解析的处理类
        if (!addCodeParserExtensions()) {
            return false;
        }

        // 调用java-callgraph2
        logger.info("调用java-callgraph2生成jar包的方法调用关系");
        boolean success = jCallGraph.run(javaCGConfigureWrapper);
        if (!success) {
            logger.error("调用java-callgraph2生成jar包的方法调用关系失败");
            return false;
        }

        // 获取输出信息
        javaCGOutputInfo = jCallGraph.getJavaCGOutputInfo();
        currentOutputDirPath = javaCGOutputInfo.getOutputDirPath();

        // 打印当前使用的配置信息
        printAllConfigInfo();

        // 打印java-callgraph2当前使用的配置信息
        printJavaCGUsedConfigInfo();
        return true;
    }

    // 添加代码解析扩展类
    private boolean addCodeParserExtensions() {
        List<String> codeParserExtensionClassList = configureWrapper.getOtherConfigList(OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_CODE_PARSER, true);
        Set<String> codeParserExtensionClassSet = new HashSet<>(codeParserExtensionClassList);
        if (codeParserExtensionClassList.size() != codeParserExtensionClassSet.size()) {
            logger.info("指定的用于对代码进行解析的扩展类存在重复 {} {}", OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_CODE_PARSER.getKey(), StringUtils.join(codeParserExtensionClassList));
            return false;
        }
        if (codeParserExtensionClassSet.contains(MyBatisMySqlSqlInfoCodeParser.class.getName())) {
            logger.info("用于对代码进行解析的扩展类不能在配置文件 {} 中指定 {}", OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_CODE_PARSER.getKey(), MyBatisMySqlSqlInfoCodeParser.class.getName());
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

        jCallGraph.addCodeParser(myBatisMySqlSqlInfoCodeParser);
        jCallGraph.addCodeParser(myBatisMySqlColumnInfoCodeParser);
        jCallGraph.addCodeParser(myBatisMySqlEntityInfoCodeParser);
        jCallGraph.addCodeParser(myBatisMySqlWriteSqlInfoCodeParser);
        jCallGraph.addCodeParser(myBatisMySqlSetColumnCodeParser);
        jCallGraph.addCodeParser(myBatisMySqlSelectColumnCodeParser);
        jCallGraph.addCodeParser(myBatisMySqlWhereColumnCodeParser);
        jCallGraph.addCodeParser(new SpringTaskXmlCodeParser());
        jCallGraph.addCodeParser(new MyBatisAnnotationCodeParser());
        jCallGraph.addCodeParser(new PropertiesConfCodeParser());
        jCallGraph.addCodeParser(new SpringXmlBeanParser());

        // 添加参数配置中的代码解析扩展类
        if (!JavaCGUtil.isCollectionEmpty(codeParserExtensionClassList)) {
            logger.info("添加参数配置中的代码解析扩展类\n{}", StringUtils.join(codeParserExtensionClassList, "\n"));
            for (String extensionClass : codeParserExtensionClassList) {
                CodeParserInterface codeParser = JACGUtil.genClassObject(extensionClass, CodeParserInterface.class);
                if (codeParser == null) {
                    return false;
                }
                // 添加代码解析扩展类
                jCallGraph.addCodeParser(codeParser);
            }
        }
        return true;
    }

    /**
     * 打印当前使用的配置信息
     */
    protected void printJavaCGUsedConfigInfo() {
        String configMdFilePath = JavaCGUtil.addSeparator4FilePath(currentOutputDirPath) + JACGConstants.FILE_JAVACG_USED_CONFIG_MD;
        logger.info("{} 全部的配置参数信息保存到以下文件 {}", JCallGraph.class.getSimpleName(), configMdFilePath);
        try (MarkdownWriter markdownWriter = new MarkdownWriter(configMdFilePath, true)) {
            // 打印java-callgraph2的主要配置信息
            printJavaCGMainConfigInfo(markdownWriter);

            // 打印Set格式的其他配置信息
            printJavaCGOtherSetConfigInfo(markdownWriter);

            // 打印List格式的其他配置信息
            printJavaCGOtherListConfigInfo(markdownWriter);
        } catch (Exception e) {
            logger.error("{} error ", currentSimpleClassName, e);
        }
    }

    // 打印java-callgraph2的主要配置信息
    private void printJavaCGMainConfigInfo(MarkdownWriter markdownWriter) throws IOException {
        // 写入配置文件名
        markdownWriter.addTitle(1, JavaCGConstants.DIR_CONFIG + "/" + JavaCGConstants.FILE_CONFIG);
        markdownWriter.addTableHead(JACGConstants.USED_CONFIG_FLAG_CONF_KEY, JACGConstants.USED_CONFIG_FLAG_CONF_DESC, JACGConstants.USED_CONFIG_FLAG_CONF_VALUE);
        for (JavaCGConfigKeyEnum javaCGConfigKeyEnum : JavaCGConfigKeyEnum.values()) {
            String value = javaCGConfigureWrapper.getConfig(null, javaCGConfigKeyEnum, false);
            markdownWriter.addTableBody(javaCGConfigKeyEnum.getKey(), javaCGConfigKeyEnum.getDesc(), (value == null ? "" : value));
        }
        markdownWriter.addEmptyLine();
    }

    // 打印List格式的其他配置信息
    private void printJavaCGOtherListConfigInfo(MarkdownWriter markdownWriter) throws IOException {
        JavaCGOtherConfigFileUseListEnum[] configs = JavaCGOtherConfigFileUseListEnum.values();
        for (int i = 0; i < configs.length; i++) {
            JavaCGOtherConfigFileUseListEnum currentConfig = configs[i];
            if (JavaCGOtherConfigFileUseListEnum.OCFULE_CODE_PARSER_ONLY_4SHOW == currentConfig) {
                // 代码解析扩展类名特殊处理
                List<String> allCodeParserNameList = jCallGraph.getAllCodeParserNameList();
                configureWrapper.doPrintListConfigInfo(markdownWriter, i, currentConfig.getFileName(), currentConfig.getDesc(), allCodeParserNameList);
                continue;
            }
            configureWrapper.doPrintListConfigInfo(markdownWriter, i, currentConfig.getFileName(), currentConfig.getDesc(), javaCGConfigureWrapper.getOtherConfigList(currentConfig
                    , false));
        }
    }

    // 打印Set格式的其他配置信息
    private void printJavaCGOtherSetConfigInfo(MarkdownWriter markdownWriter) throws IOException {
        JavaCGOtherConfigFileUseSetEnum[] configs = JavaCGOtherConfigFileUseSetEnum.values();
        for (int i = 0; i < configs.length; i++) {
            JavaCGOtherConfigFileUseSetEnum currentConfig = configs[i];
            configureWrapper.doPrintSetConfigInfo(markdownWriter, i, currentConfig.getFileName(), currentConfig.getDesc(), javaCGConfigureWrapper.getOtherConfigSet(currentConfig,
                    false));
        }
    }

    // 打印重复的类名
    protected void printDuplicateClasses() {
        Map<String, List<String>> duplicateClassNameMap = jCallGraph.getDuplicateClassNameMap();
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
