package com.adrninistrator.jacg.runner;

import com.adrninistrator.jacg.annotation.attributes.AnnotationAttributesFormatter;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.conf.enums.ConfigDbKeyEnum;
import com.adrninistrator.jacg.conf.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.extensions.codeparser.jarentryotherfile.MyBatisMySqlColumnInfoCodeParser;
import com.adrninistrator.jacg.extensions.codeparser.jarentryotherfile.MyBatisMySqlEntityInfoCodeParser;
import com.adrninistrator.jacg.extensions.codeparser.jarentryotherfile.MyBatisMySqlFormatedSqlCodeParser;
import com.adrninistrator.jacg.extensions.codeparser.jarentryotherfile.MyBatisMySqlSelectColumnCodeParser;
import com.adrninistrator.jacg.extensions.codeparser.jarentryotherfile.MyBatisMySqlSetColumnCodeParser;
import com.adrninistrator.jacg.extensions.codeparser.jarentryotherfile.MyBatisMySqlSqlInfoCodeParser;
import com.adrninistrator.jacg.extensions.codeparser.jarentryotherfile.MyBatisMySqlWhereColumnCodeParser;
import com.adrninistrator.jacg.extensions.codeparser.jarentryotherfile.MyBatisMySqlWriteSqlInfoCodeParser;
import com.adrninistrator.jacg.extensions.codeparser.jarentryotherfile.PropertiesConfCodeParser;
import com.adrninistrator.jacg.extensions.codeparser.jarentryotherfile.SpringXmlBeanParser;
import com.adrninistrator.jacg.extensions.codeparser.jarentryotherfile.SpringXmlCodeParser;
import com.adrninistrator.jacg.extensions.codeparser.methodannotation.MyBatisAnnotationCodeParser;
import com.adrninistrator.jacg.runner.base.AbstractRunner;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.javacg2.conf.JavaCG2ConfigureWrapper;
import com.adrninistrator.javacg2.conf.enums.JavaCG2ConfigKeyEnum;
import com.adrninistrator.javacg2.conf.enums.interfaces.MainConfigInterface;
import com.adrninistrator.javacg2.dto.output.JavaCG2OtherRunResult;
import com.adrninistrator.javacg2.dto.output.JavaCG2OutputInfo;
import com.adrninistrator.javacg2.entry.JavaCG2Entry;
import com.adrninistrator.javacg2.extensions.codeparser.CodeParserInterface;
import com.adrninistrator.javacg2.extensions.manager.ExtensionsManager;
import com.adrninistrator.javacg2.extensions.methodcall.JavaCG2MethodCallExtensionInterface;
import com.adrninistrator.javacg2.util.JavaCG2FileUtil;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.Writer;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2023/4/2
 * @description: 生成Java方法调用关系文件
 */
public class RunnerWriteCallGraphFile extends AbstractRunner {
    private static final Logger logger = LoggerFactory.getLogger(RunnerWriteCallGraphFile.class);

    public static final String JACG_CONFIG_FILE_NAME = "jacg_config";

    // java-callgraph2输出文件信息
    protected JavaCG2OutputInfo javaCG2OutputInfo;

    // java-callgraph2的配置包装类
    protected JavaCG2ConfigureWrapper javaCG2ConfigureWrapper;

    // java-callgraph2的入口类
    private JavaCG2Entry javaCG2Entry;

    /**
     * 构造函数，使用配置文件中的参数
     */
    public RunnerWriteCallGraphFile() {
        super();
        javaCG2ConfigureWrapper = new JavaCG2ConfigureWrapper(false);
    }

    /**
     * 构造函数，使用代码中指定的参数
     *
     * @param javaCG2ConfigureWrapper
     * @param configureWrapper
     */
    public RunnerWriteCallGraphFile(JavaCG2ConfigureWrapper javaCG2ConfigureWrapper, ConfigureWrapper configureWrapper) {
        super(configureWrapper);
        this.javaCG2ConfigureWrapper = javaCG2ConfigureWrapper;
    }

    @Override
    protected boolean preHandle() {
        return true;
    }

    @Override
    protected void handle() {
        // 调用java-callgraph2生成jar包的方法调用关系
        if (!callJavaCallGraph2()) {
            // 记录执行失败
            recordTaskFail();
        }
    }

    @Override
    protected boolean checkH2DbFile() {
        // 不检查H2数据库文件
        return true;
    }

    // 调用java-callgraph2生成jar包的方法调用关系
    protected boolean callJavaCallGraph2() {
        // 生成java-callgraph2使用的配置信息
        javaCG2Entry = new JavaCG2Entry(javaCG2ConfigureWrapper);

        ExtensionsManager extensionsManager = javaCG2Entry.getExtensionsManager();
        // 设置对注解属性进行格式化的类
        extensionsManager.setAnnotationAttributesFormatter(new AnnotationAttributesFormatter());

        // 添加用于对代码进行解析的处理类
        if (!addCodeParserExtensions(extensionsManager)) {
            return false;
        }

        // 添加 java-callgraph2 组件方法调用处理扩展类
        if (!addJavaCG2MethodCallExtensions(extensionsManager)) {
            return false;
        }

        // 调用java-callgraph2
        logger.info("调用java-callgraph2生成jar包的方法调用关系");
        boolean success = javaCG2Entry.run();
        if (!success) {
            boolean javaCG2ContinueWhenError = javaCG2ConfigureWrapper.getMainConfig(JavaCG2ConfigKeyEnum.CKE_CONTINUE_WHEN_ERROR);
            if (!javaCG2ContinueWhenError) {
                logger.error("调用java-callgraph2生成jar包的方法调用关系失败，假如需要继续，可设置 java-callgraph2 组件配置文件参数值为 {} {}", Boolean.TRUE,
                        configureWrapper.genConfigUsage(JavaCG2ConfigKeyEnum.CKE_CONTINUE_WHEN_ERROR));
                return false;
            } else {
                logger.warn("调用java-callgraph2生成jar包的方法调用关系失败");
            }
        }

        // 获取输出信息
        javaCG2OutputInfo = javaCG2Entry.getJavaCG2InputAndOutput().getJavaCG2OutputInfo();
        currentOutputDirPath = javaCG2OutputInfo.getOutputDirPath();

        // 将所有的配置信息写入文件
        if (!recordAllConfigToFile()) {
            return false;
        }

        // 打印配置信息
        return printAllConfigInfo();
    }

    // 添加代码解析扩展类
    private boolean addCodeParserExtensions(ExtensionsManager extensionsManager) {
        List<String> codeParserExtensionClassList = configureWrapper.getOtherConfigList(OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_CODE_PARSER);
        if (codeParserExtensionClassList.contains(MyBatisMySqlSqlInfoCodeParser.class.getName())) {
            logger.error("用于对代码进行解析的扩展类不能在配置文件中指定 {} {}", MyBatisMySqlSqlInfoCodeParser.class.getName(),
                    configureWrapper.genConfigUsage(OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_CODE_PARSER));
            return false;
        }

        // 添加默认的代码解析扩展类
        MyBatisMySqlSqlInfoCodeParser myBatisMySqlSqlInfoCodeParser = new MyBatisMySqlSqlInfoCodeParser();

        MyBatisMySqlColumnInfoCodeParser myBatisMySqlColumnInfoCodeParser = new MyBatisMySqlColumnInfoCodeParser();
        MyBatisMySqlEntityInfoCodeParser myBatisMySqlEntityInfoCodeParser = new MyBatisMySqlEntityInfoCodeParser();
        MyBatisMySqlFormatedSqlCodeParser myBatisMySqlFormatedSqlCodeParser = new MyBatisMySqlFormatedSqlCodeParser();
        MyBatisMySqlWriteSqlInfoCodeParser myBatisMySqlWriteSqlInfoCodeParser = new MyBatisMySqlWriteSqlInfoCodeParser();
        MyBatisMySqlSetColumnCodeParser myBatisMySqlSetColumnCodeParser = new MyBatisMySqlSetColumnCodeParser();
        MyBatisMySqlSelectColumnCodeParser myBatisMySqlSelectColumnCodeParser = new MyBatisMySqlSelectColumnCodeParser();
        MyBatisMySqlWhereColumnCodeParser myBatisMySqlWhereColumnCodeParser = new MyBatisMySqlWhereColumnCodeParser();

        myBatisMySqlSqlInfoCodeParser.setMyBatisMySqlColumnInfoCodeParser(myBatisMySqlColumnInfoCodeParser);
        myBatisMySqlSqlInfoCodeParser.setMyBatisMySqlEntityInfoCodeParser(myBatisMySqlEntityInfoCodeParser);
        myBatisMySqlSqlInfoCodeParser.setMyBatisMySqlFormatedSqlCodeParser(myBatisMySqlFormatedSqlCodeParser);
        myBatisMySqlSqlInfoCodeParser.setMyBatisMySqlWriteSqlInfoCodeParser(myBatisMySqlWriteSqlInfoCodeParser);
        myBatisMySqlSqlInfoCodeParser.setMyBatisMySqlSetColumnCodeParser(myBatisMySqlSetColumnCodeParser);
        myBatisMySqlSqlInfoCodeParser.setMyBatisMySqlSelectColumnCodeParser(myBatisMySqlSelectColumnCodeParser);
        myBatisMySqlSqlInfoCodeParser.setMyBatisMySqlWhereColumnCodeParser(myBatisMySqlWhereColumnCodeParser);

        extensionsManager.addCodeParser(myBatisMySqlSqlInfoCodeParser);
        extensionsManager.addCodeParser(myBatisMySqlColumnInfoCodeParser);
        extensionsManager.addCodeParser(myBatisMySqlEntityInfoCodeParser);
        extensionsManager.addCodeParser(myBatisMySqlFormatedSqlCodeParser);
        extensionsManager.addCodeParser(myBatisMySqlWriteSqlInfoCodeParser);
        extensionsManager.addCodeParser(myBatisMySqlSetColumnCodeParser);
        extensionsManager.addCodeParser(myBatisMySqlSelectColumnCodeParser);
        extensionsManager.addCodeParser(myBatisMySqlWhereColumnCodeParser);
        extensionsManager.addCodeParser(new SpringXmlCodeParser());
        extensionsManager.addCodeParser(new MyBatisAnnotationCodeParser());
        extensionsManager.addCodeParser(new PropertiesConfCodeParser());
        extensionsManager.setSpringXmlBeanParser(new SpringXmlBeanParser());

        // 添加参数配置中的代码解析扩展类
        if (!JavaCG2Util.isCollectionEmpty(codeParserExtensionClassList)) {
            logger.info("添加参数配置中的代码解析扩展类\n{}", StringUtils.join(codeParserExtensionClassList, "\n"));
            for (String extensionClass : codeParserExtensionClassList) {
                CodeParserInterface codeParser = JACGClassMethodUtil.genClassObject(extensionClass, CodeParserInterface.class);
                if (codeParser == null) {
                    return false;
                }
                extensionsManager.addCodeParser(codeParser);
            }
        }
        return true;
    }

    // 添加 java-callgraph2 组件方法调用处理扩展类
    private boolean addJavaCG2MethodCallExtensions(ExtensionsManager extensionsManager) {
        List<String> methodCallExtensionClassList = configureWrapper.getOtherConfigList(OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_JAVACG2_METHOD_CALL);
        Set<String> codeParserExtensionClassSet = new HashSet<>(methodCallExtensionClassList);
        if (methodCallExtensionClassList.size() != codeParserExtensionClassSet.size()) {
            logger.warn("指定的 java-callgraph2 组件方法调用处理扩展类存在重复 {} {}", StringUtils.join(methodCallExtensionClassList),
                    configureWrapper.genConfigUsage(OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_JAVACG2_METHOD_CALL));
        }

        // 添加参数配置中的代码解析扩展类
        if (!JavaCG2Util.isCollectionEmpty(methodCallExtensionClassList)) {
            logger.info("添加 java-callgraph2 组件方法调用处理扩展类\n{}", StringUtils.join(methodCallExtensionClassList, "\n"));
            for (String extensionClass : methodCallExtensionClassList) {
                JavaCG2MethodCallExtensionInterface methodCallExtension = JACGClassMethodUtil.genClassObject(extensionClass, JavaCG2MethodCallExtensionInterface.class);
                if (methodCallExtension == null) {
                    return false;
                }
                extensionsManager.addJavaCG2MethodCallExtension(methodCallExtension);
            }
        }
        return true;
    }

    // 获取java-callgraph2其他执行结果
    public JavaCG2OtherRunResult getJavaCG2OtherRunResult() {
        if (javaCG2Entry == null) {
            return new JavaCG2OtherRunResult();
        }
        return javaCG2Entry.getJavaCG2InputAndOutput().getJavaCG2OtherRunResult();
    }

    // 将所有的配置信息写入文件
    private boolean recordAllConfigToFile() {
        String filePath = JavaCG2FileUtil.genFilePath(javaCG2OutputInfo.getOutputDirPath(), JACG_CONFIG_FILE_NAME, javaCG2OutputInfo.getOutputFileExt());
        try (Writer writer = JavaCG2FileUtil.genBufferedWriter(filePath);) {
            MainConfigInterface[][] mainConfigs = new MainConfigInterface[][]{ConfigKeyEnum.values(), ConfigDbKeyEnum.values()};
            return configureWrapper.recordAllConfigToFile(writer, mainConfigs, OtherConfigFileUseListEnum.values(), OtherConfigFileUseSetEnum.values());
        } catch (Exception e) {
            logger.error("error ", e);
            return false;
        }
    }

    @Override
    protected boolean handleDb() {
        // 返回不需要操作数据库
        return false;
    }
}
