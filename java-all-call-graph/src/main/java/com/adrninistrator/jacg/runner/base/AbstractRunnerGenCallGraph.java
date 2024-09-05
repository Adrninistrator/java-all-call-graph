package com.adrninistrator.jacg.runner.base;

import com.adrninistrator.jacg.annotation.formatter.AbstractAnnotationFormatter;
import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.JACGCommonNameConstants;
import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.DefaultBusinessDataTypeEnum;
import com.adrninistrator.jacg.common.enums.MethodCallFlagsEnum;
import com.adrninistrator.jacg.common.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.common.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.common.enums.OutputDetailEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dto.annotation.BaseAnnotationAttribute;
import com.adrninistrator.jacg.dto.methodcall.ObjArgsInfoInMethodCall;
import com.adrninistrator.jacg.dto.multiple.MultiCallInfo;
import com.adrninistrator.jacg.dto.task.FindMethodTaskInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4LambdaMethodInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCall;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodLineNumber;
import com.adrninistrator.jacg.handler.annotation.AnnotationHandler;
import com.adrninistrator.jacg.handler.dto.businessdata.BaseBusinessData;
import com.adrninistrator.jacg.handler.dto.genericstype.GenericsTypeValue;
import com.adrninistrator.jacg.handler.dto.genericstype.MethodArgGenericsTypeInfo;
import com.adrninistrator.jacg.handler.lambda.LambdaMethodHandler;
import com.adrninistrator.jacg.handler.method.MethodArgReturnHandler;
import com.adrninistrator.jacg.handler.methodcall.MethodCallHandler;
import com.adrninistrator.jacg.handler.methodcall.MethodCallInfoHandler;
import com.adrninistrator.jacg.handler.mybatis.MyBatisMSMapperEntityHandler;
import com.adrninistrator.jacg.markdown.enums.MDCodeBlockTypeEnum;
import com.adrninistrator.jacg.markdown.writer.MarkdownWriter;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Callee;
import com.adrninistrator.jacg.runner.RunnerWriteDb;
import com.adrninistrator.jacg.util.JACGCallGraphFileUtil;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGJsonUtil;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg2.common.JavaCG2CommonNameConstants;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.common.enums.JavaCG2CallTypeEnum;
import com.adrninistrator.javacg2.common.enums.JavaCG2YesNoEnum;
import com.adrninistrator.javacg2.util.JavaCG2FileUtil;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

/**
 * @author adrninistrator
 * @date 2021/6/18
 * @description:
 */

public abstract class AbstractRunnerGenCallGraph extends AbstractRunner {
    private static final Logger logger = LoggerFactory.getLogger(AbstractRunnerGenCallGraph.class);

    // 当前生成的完整方法调用链方向是否为向上
    protected final boolean order4ee = this instanceof RunnerGenAllGraph4Callee;

    // 配置文件中指定的需要处理的任务
    protected Set<String> taskSet;

    // 当方法调用类型在以下Set中时，生成方法完整调用链时忽略
    protected Set<String> ignoreCallTypeSet;

    // 当类名包含以下关键字时，生成方法完整调用链时忽略
    protected Set<String> ignoreClassKeywordSet;

    // 完整方法（类名+方法名+参数）为以下前缀时，生成方法完整调用链时忽略
    protected Set<String> ignoreFullMethodPrefixSet;

    // 当方法名为以下前缀时，生成方法完整调用链时忽略
    protected Set<String> ignoreMethodPrefixSet;

    // 完整方法（类名+方法名+参数）为以下前缀时，生成方法完整调用链时包含
    protected Set<String> includeFullMethodPrefixSet;

    // 需要显示的业务功能数据类型，Set格式
    protected Set<String> businessDataTypeSet;

    // 需要显示的业务功能数据类型，List格式
    protected List<String> businessDataTypeList;

    protected AnnotationHandler annotationHandler;
    protected MethodCallHandler methodCallHandler;
    protected MethodCallInfoHandler methodCallInfoHandler;
    protected MethodArgReturnHandler methodArgReturnHandler;
    protected MyBatisMSMapperEntityHandler myBatisMSMapperEntityHandler;
    protected LambdaMethodHandler lambdaMethodHandler;

    /*
        接口调用对应实现类的方法调用
        key: 接口方法
        value: 实现类方法
     */
    private final Map<String, MultiCallInfo> itfMethodCallMap = new HashMap<>();

    /*
        抽象父类调用对应子类的方法调用
        key: 抽象父类方法
        value: 子类方法
     */
    private final Map<String, MultiCallInfo> sccMethodCallMap = new HashMap<>();

    // 接口调用对应实现类的方法调用，存在一对多的接口
    private final Set<String> itfMultiCallerFullMethodSet = ConcurrentHashMap.newKeySet();

    // 抽象父类调用对应子类的方法调用，存在一对多的抽象父类
    private final Set<String> sccMultiCallerFullMethodSet = ConcurrentHashMap.newKeySet();

    /*
        被禁用的接口调用对应实现类的方法调用
        key: 接口方法
        value: 实现类方法
     */
    private final Map<String, MultiCallInfo> disabledItfMethodCallMap = new ConcurrentHashMap<>();

    /*
        被禁用的抽象父类调用对应子类的方法调用
        key: 抽象父类方法
        value: 子类方法
     */
    private final Map<String, MultiCallInfo> disabledSccMethodCallMap = new ConcurrentHashMap<>();

    // 保存用于对方法上的注解进行处理的类
    protected List<AbstractAnnotationFormatter> annotationFormatterList;

    // 保存各个方法已处理过的所有注解信息
    protected Map<String, String> methodAllAnnotationInfoMap = new ConcurrentHashMap<>();

    // 保存已生成的过方法文件名
    protected Set<String> writtenFileNameSet = ConcurrentHashMap.newKeySet();

    // 输出结果展示详细程度枚举
    protected OutputDetailEnum outputDetailEnum;

    public AbstractRunnerGenCallGraph() {
        super();
    }

    public AbstractRunnerGenCallGraph(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
    }

    protected boolean handleDb() {
        return true;
    }

    // 公共预处理
    protected boolean commonPreHandle() {
        outputDetailEnum = OutputDetailEnum.getFromDetail(configureWrapper.getMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_OUTPUT_DETAIL));

        // 从数据库查询数据需要在以上检查H2数据库文件之后
        if (!dbOperWrapper.findDuplicateClass()) {
            return false;
        }

        if (!useNeo4j()) {
            // 初始相关处理类
            annotationHandler = new AnnotationHandler(dbOperWrapper);
            methodCallHandler = new MethodCallHandler(dbOperWrapper);
            lambdaMethodHandler = new LambdaMethodHandler(dbOperWrapper);

            if (!Boolean.TRUE.equals(configureWrapper.getMainConfig(ConfigKeyEnum.CKE_CHECK_JAR_FILE_UPDATED))) {
                logger.info("不检查jar包文件是否有变化");
            } else {
                // 检查jar包文件是否有变化
                List<String> jarPathList = getJarPathList();
                if (checkSomeJarModified(jarPathList)) {
                    logger.error("请先执行 {} 类导入数据库\n假如不需要检查jar包文件是否有变化，可修改配置文件 {} 参数值 {} 为 {}",
                            RunnerWriteDb.class.getSimpleName(), ConfigKeyEnum.CKE_CHECK_JAR_FILE_UPDATED.getFileName(),
                            ConfigKeyEnum.CKE_CHECK_JAR_FILE_UPDATED.getConfigPrintInfo(), Boolean.FALSE);
                    return false;
                }
            }

            // 检查允许处理的类名或包名前缀是否有变化
            if (checkAllowedClassPrefixModified()) {
                return false;
            }

            // 添加用于添加对方法上的注解进行处理的类
            if (!addMethodAnnotationHandlerExtensions()) {
                return false;
            }

            // 初始化默认的处理业务功能数据的类
            if (!initDefaultBusinessDataHandler()) {
                return false;
            }
        }

        // 获取生成方法完整调用链时需要忽略的信息
        if (!initIgnoreInfo()) {
            return false;
        }
        return true;
    }

    // 获取生成方法完整调用链时需要忽略的信息
    protected boolean initIgnoreInfo() {
        ignoreCallTypeSet = configureWrapper.getOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_IGNORE_CALL_TYPE, true);
        if (ignoreCallTypeSet == null) {
            return false;
        }

        ignoreClassKeywordSet = configureWrapper.getOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_IGNORE_CLASS_KEYWORD, true);
        if (ignoreClassKeywordSet == null) {
            return false;
        }

        ignoreFullMethodPrefixSet = configureWrapper.getOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_IGNORE_FULL_METHOD_PREFIX, true);
        if (ignoreFullMethodPrefixSet == null) {
            return false;
        }

        ignoreMethodPrefixSet = configureWrapper.getOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_IGNORE_METHOD_PREFIX, true);
        if (ignoreMethodPrefixSet == null) {
            return false;
        }

        includeFullMethodPrefixSet = configureWrapper.getOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_INCLUDE_FULL_METHOD_PREFIX, true);
        if (includeFullMethodPrefixSet == null) {
            return false;
        }
        return true;
    }

    // 读取配置文件中指定的需要处理的任务
    protected boolean readTaskInfo(OtherConfigFileUseSetEnum otherConfigFileUseSetEnum) {
        taskSet = configureWrapper.getOtherConfigSet(otherConfigFileUseSetEnum, true);
        if (JavaCG2Util.isCollectionEmpty(taskSet)) {
            logger.error("读取文件不存在或内容为空 {}", otherConfigFileUseSetEnum.getConfigPrintInfo());
            return false;
        }

        return true;
    }

    /**
     * 创建输出文件所在目录
     * 需要进行同步控制，避免创建同名目录
     *
     * @param prefix 代表向上或向下的目录名
     * @return
     */
    protected boolean createOutputDir(String prefix) {
        synchronized (AbstractRunnerGenCallGraph.class) {
            String outputDirPrefix;
            String outputRootPathInProperties = configureWrapper.getMainConfig(ConfigKeyEnum.CKE_OUTPUT_ROOT_PATH);
            if (StringUtils.isNotBlank(outputRootPathInProperties)) {
                // 使用指定的生成结果文件根目录，并指定当前应用名称
                outputDirPrefix = JavaCG2Util.addSeparator4FilePath(outputRootPathInProperties) + prefix + File.separator;
            } else {
                // 使用当前目录作为生成结果文件根目录
                outputDirPrefix = prefix + File.separator;
            }

            String outputDirName = configureWrapper.getMainConfig(ConfigKeyEnum.CKE_OUTPUT_DIR_NAME);
            if (StringUtils.isNotBlank(outputDirName)) {
                // 使用指定的名称作为目录名
                outputDirPrefix += outputDirName;
            } else {
                // 完整目录名使用{app.name}{output.dir.flag}_{当前时间}
                String outputDirFlag = configureWrapper.getMainConfig(ConfigKeyEnum.CKE_OUTPUT_DIR_FLAG);
                outputDirPrefix = outputDirPrefix + appName + outputDirFlag + JACGConstants.FLAG_UNDER_LINE + JavaCG2Util.currentTime();
            }

            File currentOutputDir = new File(outputDirPrefix);
            currentOutputDirPath = currentOutputDir.getAbsolutePath();
            logger.info("创建保存输出文件的目录 {}", currentOutputDirPath);

            if (StringUtils.isNotBlank(outputDirName) && currentOutputDir.exists()) {
                logger.error("指定的输出目录已存在，若确实需要在该目录中输出，请先删除该目录\n{} {} {}\n{}", ConfigKeyEnum.CKE_OUTPUT_DIR_NAME.getFileName(),
                        ConfigKeyEnum.CKE_OUTPUT_DIR_NAME.getConfigPrintInfo(), outputDirName, currentOutputDirPath);
                return false;
            }

            // 判断目录是否存在，不存在时尝试创建
            if (!JavaCG2FileUtil.isDirectoryExists(currentOutputDirPath)) {
                return false;
            }
        }

        // 打印当前使用的配置信息
        printAllConfigInfo();
        return true;
    }

    // 根据调用关系ID获取用于提示的信息
    private WriteDbData4MethodCall queryNoticeCallInfo(int currentMethodCallId) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MC_QUERY_NOTICE_INFO;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.joinColumns(DC.MC_CALLER_METHOD_HASH, DC.MC_CALLER_FULL_METHOD, DC.MC_CALLEE_FULL_METHOD) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName() +
                    " where " + DC.MC_CALL_ID + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        WriteDbData4MethodCall methodCall = dbOperator.queryObject(sql, WriteDbData4MethodCall.class, currentMethodCallId);
        if (methodCall == null) {
            logger.error("查询需要提示的信息失败 {}", currentMethodCallId);
        }
        return methodCall;
    }

    // 记录可能出现一对多的方法调用
    protected boolean recordMethodCallMayBeMulti(int currentMethodCallId, String callType) {
        JavaCG2CallTypeEnum callTypeEnum = JavaCG2CallTypeEnum.getFromType(callType);
        if (callTypeEnum != JavaCG2CallTypeEnum.CTE_INTERFACE_CALL_IMPL_CLASS && callTypeEnum != JavaCG2CallTypeEnum.CTE_SUPER_CALL_CHILD) {
            // 对于接口调用实现类、父类调用子类之外的情况，不判断是否出现出现一对多的方法调用
            return true;
        }

        // 对于接口调用实现类、父类调用子类，判断是否出现出现一对多的方法调用
        // 根据调用关系ID获取用于提示的信息
        WriteDbData4MethodCall methodCall = queryNoticeCallInfo(currentMethodCallId);
        if (methodCall == null) {
            return false;
        }

        Map<String, MultiCallInfo> methodCallMap;
        Set<String> multiCallerFullMethodSet;

        if (callTypeEnum == JavaCG2CallTypeEnum.CTE_INTERFACE_CALL_IMPL_CLASS) {
            methodCallMap = itfMethodCallMap;
            multiCallerFullMethodSet = itfMultiCallerFullMethodSet;
        } else {
            methodCallMap = sccMethodCallMap;
            multiCallerFullMethodSet = sccMultiCallerFullMethodSet;
        }

        String callerMethodHash = methodCall.getCallerMethodHash();
        String callerFullMethod = methodCall.getCallerFullMethod();
        String calleeFullMethod = methodCall.getCalleeFullMethod();

        // 以下对Map及Set的处理会并发执行，需要串行执行，避免添加的数据丢失
        synchronized (AbstractRunnerGenCallGraph.class) {
            MultiCallInfo multiCallInfo = methodCallMap.computeIfAbsent(callerFullMethod, k -> new MultiCallInfo(callerMethodHash, new HashSet<>()));
            Set<String> calleeMethodSet = multiCallInfo.getCalleeFullMethodSet();
            calleeMethodSet.add(calleeFullMethod);
            if (calleeMethodSet.size() > 1) {
                multiCallerFullMethodSet.add(callerFullMethod);
            }
        }

        return true;
    }

    // 记录被禁用的方法调用
    protected boolean recordDisabledMethodCall(int callId, String callType) {
        JavaCG2CallTypeEnum callTypeEnum = JavaCG2CallTypeEnum.getFromType(callType);
        if (callTypeEnum != JavaCG2CallTypeEnum.CTE_INTERFACE_CALL_IMPL_CLASS && callTypeEnum != JavaCG2CallTypeEnum.CTE_SUPER_CALL_CHILD) {
            // 对于被禁用的方法调用，仅对接口调用实现类，及父类调用子类的情况提示
            return true;
        }

        // 根据调用关系ID获取用于提示的信息
        WriteDbData4MethodCall methodCall = queryNoticeCallInfo(callId);
        if (methodCall == null) {
            return false;
        }

        Map<String, MultiCallInfo> methodCallMap;

        if (callTypeEnum == JavaCG2CallTypeEnum.CTE_INTERFACE_CALL_IMPL_CLASS) {
            methodCallMap = disabledItfMethodCallMap;
        } else {
            methodCallMap = disabledSccMethodCallMap;
        }

        String callerMethodHash = methodCall.getCallerMethodHash();
        String callerFullMethod = methodCall.getCallerFullMethod();
        String calleeFullMethod = methodCall.getCalleeFullMethod();

        MultiCallInfo multiCallInfo = methodCallMap.computeIfAbsent(callerFullMethod, k -> new MultiCallInfo(callerMethodHash, ConcurrentHashMap.newKeySet()));
        multiCallInfo.getCalleeFullMethodSet().add(calleeFullMethod);
        return true;
    }

    // 打印存在一对多的方法调用
    private void printMultiMethodCall(Map<String, MultiCallInfo> methodCallMap, Set<String> multiCallerFullMethodSet, JavaCG2CallTypeEnum callTypeEnum) {
        // 判断相关存在一对多的调用方方法是否有被其他方法调用，若未被调用则不显示
        List<String> multiCallerFullMethodList = new ArrayList<>(multiCallerFullMethodSet.size());
        for (String multiCallerFullMethod : multiCallerFullMethodSet) {
            String multiCallerMethodHash = JACGUtil.genHashWithLen(multiCallerFullMethod);
            if (methodCallHandler.checkExistsNormalMethodCallByCalleeMethodHash(multiCallerMethodHash)) {
                // 当前存在一对多的调用方方法有被其他方法调用
                multiCallerFullMethodList.add(multiCallerFullMethod);
            } else {
                logger.warn("当前存在一对多的调用方方法未被其他方法调用，不打印到文件中 {}", multiCallerFullMethod);
            }
        }

        if (multiCallerFullMethodList.isEmpty()) {
            logger.info("{} 不存在一对多的方法调用，不打印相关信息", callTypeEnum);
            return;
        }

        String filePath;
        if (JavaCG2CallTypeEnum.CTE_INTERFACE_CALL_IMPL_CLASS == callTypeEnum) {
            filePath = currentOutputDirPath + File.separator + JACGConstants.NOTICE_MULTI_ITF_MD;
        } else {
            filePath = currentOutputDirPath + File.separator + JACGConstants.NOTICE_MULTI_SCC_MD;
        }

        logger.info("{} 存在一对多的方法调用，打印相关信息 {}", callTypeEnum, filePath);
        try (MarkdownWriter markdownWriter = new MarkdownWriter(filePath, true)) {
            markdownWriter.addTitle(1, "说明");

            if (JavaCG2CallTypeEnum.CTE_INTERFACE_CALL_IMPL_CLASS == callTypeEnum) {
                markdownWriter.addLineWithNewLine("出现当前文件，说明接口调用对应实现类的方法调用存在一对多的方法调用");
            } else {
                markdownWriter.addLineWithNewLine("出现当前文件，说明抽象父类调用对应子类的方法调用存在一对多的方法调用");
            }

            markdownWriter.addLineWithNewLine("可以使用以下SQL语句查找对应的方法调用并禁用，仅保留需要的调用关系");
            markdownWriter.addCodeBlock(MDCodeBlockTypeEnum.MDCBTE_SQL);
            // 生成提示信息中的查询SQL
            markdownWriter.addLine(genNoticeSelectSql(callTypeEnum.getType()));
            // 生成提示信息中的变化为禁用SQL
            markdownWriter.addLine(genNoticeUpdateDisableSql(callTypeEnum.getType()));

            if (order4ee) {
                // 生成向上的方法调用完整调用链时，增加一个显示的update语句
                markdownWriter.addEmptyLine();
                markdownWriter.addLine(genNoticeSelectSql4Callee());
                markdownWriter.addLine(genNoticeUpdateDisableSql4Callee());
            }
            markdownWriter.addCodeBlock();

            Collections.sort(multiCallerFullMethodList);
            for (String multiCallerFullMethod : multiCallerFullMethodList) {
                MultiCallInfo multiCallInfo = methodCallMap.get(multiCallerFullMethod);
                if (multiCallInfo == null || JavaCG2Util.isCollectionEmpty(multiCallInfo.getCalleeFullMethodSet())) {
                    logger.error("未查找到对应的一对多方法调用关系 {}", multiCallerFullMethod);
                    continue;
                }

                markdownWriter.addTitle(2, multiCallerFullMethod);
                markdownWriter.addListWithNewLine(DC.MC_CALLER_METHOD_HASH);
                markdownWriter.addLineWithNewLine(multiCallInfo.getCallerMethodHash());
                markdownWriter.addListWithNewLine(DC.MC_CALLEE_FULL_METHOD + "（被调用的方法）");
                markdownWriter.addCodeBlock();
                List<String> calleeFullMethodList = new ArrayList<>(multiCallInfo.getCalleeFullMethodSet());
                Collections.sort(calleeFullMethodList);
                for (String calleeMethod : calleeFullMethodList) {
                    markdownWriter.addLine(calleeMethod);
                }
                markdownWriter.addCodeBlock();

                // 打印存在一对多的方法调用，自定义处理
                printMultiMethodCallCustom(multiCallInfo.getCallerMethodHash(), markdownWriter);
            }
        } catch (Exception e) {
            logger.error("error ", e);
        }
    }

    // 打印存在一对多的方法调用，自定义处理
    protected void printMultiMethodCallCustom(String callerMethodHash, MarkdownWriter markdownWriter) throws IOException {
    }

    // 打印被禁用的方法调用
    private void printDisabledMethodCall(Map<String, MultiCallInfo> disabledMethodCallMap, JavaCG2CallTypeEnum callTypeEnum) {
        if (disabledMethodCallMap.isEmpty()) {
            logger.info("{} 不存在被禁用的方法调用，不打印相关信息", callTypeEnum);
            return;
        }

        String filePath;
        if (JavaCG2CallTypeEnum.CTE_INTERFACE_CALL_IMPL_CLASS == callTypeEnum) {
            filePath = currentOutputDirPath + File.separator + JACGConstants.NOTICE_DISABLED_ITF_MD;
        } else {
            filePath = currentOutputDirPath + File.separator + JACGConstants.NOTICE_DISABLED_SCC_MD;
        }

        logger.info("{} 存在被禁用的方法调用，打印相关信息 {}", callTypeEnum, filePath);

        try (MarkdownWriter markdownWriter = new MarkdownWriter(filePath, true)) {
            markdownWriter.addTitle(1, "说明");

            if (JavaCG2CallTypeEnum.CTE_INTERFACE_CALL_IMPL_CLASS == callTypeEnum) {
                markdownWriter.addLineWithNewLine("出现当前文件，说明接口调用对应实现类的方法调用存在被禁用的方法调用");
            } else {
                markdownWriter.addLineWithNewLine("出现当前文件，说明抽象父类调用对应子类的方法调用存在被禁用的方法调用");
            }
            markdownWriter.addLineWithNewLine("可以使用以下SQL语句查找对应的方法调用并启用");
            markdownWriter.addCodeBlock(MDCodeBlockTypeEnum.MDCBTE_SQL);

            // 生成提示信息中的查询SQL
            markdownWriter.addLineWithNewLine(genNoticeSelectSql(callTypeEnum.getType()));
            // 生成提示信息中的变化为禁用SQL
            markdownWriter.addLine(genNoticeUpdateEnableSql(callTypeEnum.getType()));
            markdownWriter.addCodeBlock();

            List<String> disabledCallerMethodList = new ArrayList<>(disabledMethodCallMap.keySet());
            Collections.sort(disabledCallerMethodList);
            for (String disabledCallerMethod : disabledCallerMethodList) {
                MultiCallInfo multiCallInfo = disabledMethodCallMap.get(disabledCallerMethod);
                if (multiCallInfo == null || JavaCG2Util.isCollectionEmpty(multiCallInfo.getCalleeFullMethodSet())) {
                    logger.error("未查找到对应的被禁用方法调用关系 {}", disabledCallerMethod);
                    continue;
                }
                markdownWriter.addTitle(2, disabledCallerMethod);
                markdownWriter.addListWithNewLine(DC.MC_CALLER_METHOD_HASH);
                markdownWriter.addLineWithNewLine(multiCallInfo.getCallerMethodHash());
                markdownWriter.addListWithNewLine(DC.MC_CALLEE_FULL_METHOD + "（被调用的方法）");

                markdownWriter.addCodeBlock();
                for (String calleeMethod : multiCallInfo.getCalleeFullMethodSet()) {
                    markdownWriter.addLine(calleeMethod);
                }
                markdownWriter.addCodeBlock();
            }
        } catch (Exception e) {
            logger.error("error ", e);
        }
    }

    // 打印提示信息
    protected void printNoticeInfo() {
        printMultiMethodCall(itfMethodCallMap, itfMultiCallerFullMethodSet, JavaCG2CallTypeEnum.CTE_INTERFACE_CALL_IMPL_CLASS);
        printMultiMethodCall(sccMethodCallMap, sccMultiCallerFullMethodSet, JavaCG2CallTypeEnum.CTE_SUPER_CALL_CHILD);
        printDisabledMethodCall(disabledItfMethodCallMap, JavaCG2CallTypeEnum.CTE_INTERFACE_CALL_IMPL_CLASS);
        printDisabledMethodCall(disabledSccMethodCallMap, JavaCG2CallTypeEnum.CTE_SUPER_CALL_CHILD);
    }

    // 生成提示信息中的查询SQL
    private String genNoticeSelectSql(String callType) {
        String sql = "select * from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName() +
                " where " + DC.MC_CALL_TYPE + " = '" + callType +
                "' and " + DC.MC_CALLER_METHOD_HASH + " = '';";
        return JACGSqlUtil.replaceFlagInSql(sql, appName, tableSuffix);
    }

    private String genNoticeSelectSql4Callee() {
        String sql = "select * from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName() +
                " where " + DC.MC_CALLEE_METHOD_HASH + " = '';";
        return JACGSqlUtil.replaceFlagInSql(sql, appName, tableSuffix);
    }

    // 生成提示信息中的变化为禁用SQL
    private String genNoticeUpdateDisableSql(String callType) {
        String sql = "update " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName() +
                " set " + DC.MC_ENABLED + " = " + JavaCG2YesNoEnum.NO.getStrValue() +
                " where " + DC.MC_CALL_TYPE + " = '" + callType +
                "' and " + DC.MC_CALLER_METHOD_HASH + " = '' and " + DC.MC_CALLEE_FULL_METHOD + " <> '';";
        return JACGSqlUtil.replaceFlagInSql(sql, appName, tableSuffix);
    }

    private String genNoticeUpdateDisableSql4Callee() {
        String sql = "update " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName() +
                " set " + DC.MC_ENABLED + " = " + JavaCG2YesNoEnum.NO.getStrValue() +
                " where " + DC.MC_CALLEE_METHOD_HASH + " = '' and " + DC.MC_CALLER_FULL_METHOD + " <> '';";
        return JACGSqlUtil.replaceFlagInSql(sql, appName, tableSuffix);
    }

    // 生成提示信息中的变化为启用SQL
    private String genNoticeUpdateEnableSql(String callType) {
        String sql = "update " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName() +
                " set " + DC.MC_ENABLED + " = " + JavaCG2YesNoEnum.YES.getStrValue() +
                " where " + DC.MC_CALL_TYPE + " = '" + callType +
                "' and " + DC.MC_CALLER_METHOD_HASH + " = '' and " + DC.MC_CALLEE_FULL_METHOD + " = '';";
        return JACGSqlUtil.replaceFlagInSql(sql, appName, tableSuffix);
    }

    @Override
    protected boolean checkH2DbFile() {
        File h2DbFile = getH2DbFile();
        if (!h2DbFile.exists()) {
            logger.error("H2数据库文件不存在，请先执行 {} 类导入数据库 {}", RunnerWriteDb.class.getSimpleName(), JavaCG2FileUtil.getCanonicalPath(h2DbFile));
            return false;
        }

        // 数据库文件存在
        if (!h2DbFile.isFile()) {
            logger.error("H2数据库文件不是文件 {}", JavaCG2FileUtil.getCanonicalPath(h2DbFile));
            return false;
        }

        // 检查H2数据库文件是否可写
        return checkH2DbFileWritable(h2DbFile);
    }

    /**
     * 获取本次执行时的输出目录
     *
     * @return
     */
    public String getCurrentOutputDirPath() {
        return currentOutputDirPath;
    }

    // 添加用于添加对方法上的注解进行处理的类
    protected boolean addMethodAnnotationHandlerExtensions() {
        List<String> methodAnnotationHandlerClassList = configureWrapper.getOtherConfigList(OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_METHOD_ANNOTATION_FORMATTER, true);
        if (JavaCG2Util.isCollectionEmpty(methodAnnotationHandlerClassList)) {
            annotationFormatterList = Collections.emptyList();
            return true;
        }

        annotationFormatterList = new ArrayList<>(methodAnnotationHandlerClassList.size());
        try {
            for (String extensionClass : methodAnnotationHandlerClassList) {
                AbstractAnnotationFormatter annotationFormatter = JACGUtil.genClassObject(extensionClass, AbstractAnnotationFormatter.class);
                if (annotationFormatter == null) {
                    return false;
                }
                annotationFormatter.setAnnotationHandler(annotationHandler);
                annotationFormatterList.add(annotationFormatter);
            }
            return true;
        } catch (Exception e) {
            logger.error("error ", e);
            return false;
        }
    }

    // 初始化默认的处理业务功能数据的类
    private boolean initDefaultBusinessDataHandler() {
        businessDataTypeSet = configureWrapper.getOtherConfigSet(order4ee ? OtherConfigFileUseSetEnum.OCFULE_BUSINESS_DATA_TYPE_SHOW_4EE :
                OtherConfigFileUseSetEnum.OCFULE_BUSINESS_DATA_TYPE_SHOW_4ER, true);
        if (businessDataTypeSet.isEmpty()) {
            // 没有指定需要处理的业务功能数据类型
            businessDataTypeList = Collections.emptyList();
            return true;
        }

        // 处理默认的业务功能数据类型
        for (String businessDataType : businessDataTypeSet) {
            if (StringUtils.containsAny(businessDataType, JavaCG2Constants.FLAG_TAB, JACGConstants.FLAG_AT)) {
                logger.error("当前指定的业务功能数据类型不允许出现以下字段，请删除 {} {}", JavaCG2Constants.FLAG_TAB, JACGConstants.FLAG_AT);
                return false;
            }

            DefaultBusinessDataTypeEnum businessDataTypeEnum = DefaultBusinessDataTypeEnum.getFromType(businessDataType);
            if (businessDataTypeEnum == null) {
                continue;
            }
            // 找到默认的业务功能数据类型
            if (order4ee && !businessDataTypeEnum.isSupportEe()) {
                logger.error("{} 当前指定的业务功能数据类型 {} 不支持在生成向上的完整方法调用链时显示，请删除", OtherConfigFileUseSetEnum.OCFULE_BUSINESS_DATA_TYPE_SHOW_4EE, businessDataTypeEnum);
                return false;
            }
            if (!order4ee && !businessDataTypeEnum.isSupportEr()) {
                logger.error("{} 当前指定的业务功能数据类型 {} 不支持在生成向下的完整方法调用链时显示，请删除", OtherConfigFileUseSetEnum.OCFULE_BUSINESS_DATA_TYPE_SHOW_4ER, businessDataTypeEnum);
                return false;
            }
        }

        if (businessDataTypeSet.contains(DefaultBusinessDataTypeEnum.BDTE_METHOD_CALL_INFO.getType())) {
            // 初始化方法调用信息处理类
            methodCallInfoHandler = new MethodCallInfoHandler(dbOperWrapper);
        }

        if (businessDataTypeSet.contains(DefaultBusinessDataTypeEnum.BDTE_MYBATIS_MYSQL_TABLE.getType()) || businessDataTypeSet.contains(DefaultBusinessDataTypeEnum.BDTE_MYBATIS_MYSQL_WRITE_TABLE.getType())) {
            // 初始化对MyBatis Mapper的处理类
            myBatisMSMapperEntityHandler = new MyBatisMSMapperEntityHandler(dbOperWrapper);
        }

        if (businessDataTypeSet.contains(DefaultBusinessDataTypeEnum.BDTE_METHOD_ARG_GENERICS_TYPE.getType()) ||
                businessDataTypeSet.contains(DefaultBusinessDataTypeEnum.BDTE_METHOD_RETURN_GENERICS_TYPE.getType())) {
            // 初始化方法参数泛型类型处理类
            methodArgReturnHandler = new MethodArgReturnHandler(dbOperWrapper);
        }

        businessDataTypeList = new ArrayList<>(businessDataTypeSet);
        Collections.sort(businessDataTypeList);
        return true;
    }

    /**
     * 获取方法对应的注解信息
     *
     * @param fullMethod              完整方法
     * @param methodHash              完整方法HASH+长度
     * @param formattedAnnotationInfo 保存格式化后的注解信息
     * @return 当前方法上的注解信息
     */
    protected Map<String, Map<String, BaseAnnotationAttribute>> getMethodAnnotationInfo(String fullMethod, String methodHash, StringBuilder formattedAnnotationInfo) {
        // 根据完整方法HASH+长度获取对应的注解信息
        Map<String, Map<String, BaseAnnotationAttribute>> methodAnnotationMap = annotationHandler.queryAnnotationMap4FullMethod(fullMethod);
        if (methodAnnotationMap == null) {
            // 当前方法上没有注解
            return null;
        }

        // 当前方法上有注解
        String existedAnnotationInfo = methodAllAnnotationInfoMap.get(methodHash);
        if (existedAnnotationInfo != null) {
            // 当前方法对应的注解信息已查询过，直接使用
            formattedAnnotationInfo.append(existedAnnotationInfo);
            return methodAnnotationMap;
        }

        // 当前方法对应的注解信息未查询过
        StringBuilder stringBuilder = new StringBuilder();

        // 遍历当前方法上的所有注解进行处理
        for (Map.Entry<String, Map<String, BaseAnnotationAttribute>> methodAnnotationMapEntry : methodAnnotationMap.entrySet()) {
            String annotationName = methodAnnotationMapEntry.getKey();
            // 遍历用于对方法上的注解进行处理的类
            for (AbstractAnnotationFormatter annotationFormatter : annotationFormatterList) {
                if (!annotationFormatter.checkHandleAnnotation(annotationName)) {
                    continue;
                }

                String className = JACGClassMethodUtil.getClassNameFromMethod(fullMethod);
                // 找到能够处理的类进行处理
                String annotationInfo = annotationFormatter.handleAnnotation(fullMethod, className, annotationName, methodAnnotationMapEntry.getValue());
                if (annotationInfo != null) {
                    // 替换TAB、回车、换行等字符，再将半角的@替换为全角，避免影响通过@对注解进行分隔
                    String finalAnnotationInfo = JACGCallGraphFileUtil.replaceSplitChars(annotationInfo)
                            .replace(JACGConstants.FLAG_AT, JACGConstants.FLAG_AT_FULL_WIDTH);

                    // 注解信息以@开头，在以上方法中不需要返回以@开头
                    stringBuilder.append(JACGConstants.FLAG_AT).append(finalAnnotationInfo);
                }
                break;
            }
        }

        String allAnnotationInfo = stringBuilder.toString();
        methodAllAnnotationInfoMap.putIfAbsent(methodHash, allAnnotationInfo);
        formattedAnnotationInfo.append(allAnnotationInfo);
        return methodAnnotationMap;
    }

    /**
     * 通过代码行号获取对应方法
     *
     * @param isCallee        true: 生成向上的方法调用链 false: 生成向下的方法调用链
     * @param simpleClassName
     * @param methodLineNum
     * @return
     */
    protected FindMethodTaskInfo findMethodByLineNumber(boolean isCallee, String simpleClassName, int methodLineNum) {
        WriteDbData4MethodLineNumber writeDbData4MethodLineNumber = dbOperWrapper.queryMethodLineNumber(simpleClassName, methodLineNum);
        if (writeDbData4MethodLineNumber == null) {
            logger.warn("指定类的代码行号未查找到对应方法，请检查，可能因为以下原因\n" +
                    "1. 指定的类所在的jar包未在配置文件 {} 中指定\n" +
                    "2. 指定的方法是接口中未实现的方法\n" +
                    "3. 指定的方法是抽象方法\n" +
                    "{} {}", OtherConfigFileUseListEnum.OCFULE_JAR_DIR.getConfigPrintInfo(), simpleClassName, methodLineNum);
            return FindMethodTaskInfo.genFindMethodInfoGenEmptyFile();
        }

        // 查询方法的标志
        WriteDbData4MethodCall methodCallExtraInfo = queryMethodCallExtraInfo(isCallee, writeDbData4MethodLineNumber.getMethodHash());
        String methodReturnType = isCallee ? methodCallExtraInfo.getRawReturnType() : methodCallExtraInfo.getCallerReturnType();
        // 指定类的代码行号查找到对应方法
        FindMethodTaskInfo findMethodTaskInfo = FindMethodTaskInfo.genFindMethodInfoSuccess();
        findMethodTaskInfo.addTaskElement(writeDbData4MethodLineNumber.getMethodHash(), writeDbData4MethodLineNumber.getFullMethod(), methodCallExtraInfo.getCallFlags(),
                methodReturnType);
        return findMethodTaskInfo;
    }

    /**
     * 查询方法调用额外信息，包括调用方法返回类型、方法调用标志、被调用方法原始返回类型等
     *
     * @param isCallee   true: 生成向上的方法调用链 false: 生成向下的方法调用链
     * @param methodHash
     * @return
     */
    private WriteDbData4MethodCall queryMethodCallExtraInfo(boolean isCallee, String methodHash) {
        // 查询方法调用的额外信息
        WriteDbData4MethodCall writeDbData4MethodCall = dbOperWrapper.queryMethodCallExtraInfo(isCallee, methodHash);
        if (writeDbData4MethodCall == null) {
            writeDbData4MethodCall = new WriteDbData4MethodCall();
            writeDbData4MethodCall.setCallerReturnType("");
            writeDbData4MethodCall.setCallFlags(0);
            writeDbData4MethodCall.setRawReturnType("");
        }
        return writeDbData4MethodCall;
    }

    /**
     * 判断方法调用类型是否需要忽略
     *
     * @param callType 方法调用类型
     * @return true: 忽略，false: 需要处理
     */
    protected boolean isIgnoredCallType(String callType) {
        return ignoreCallTypeSet.contains(callType);
    }

    /**
     * 根据前缀判断完整方法（类名+方法名+参数）是否需要忽略
     *
     * @param fullMethod 完整方法（类名+方法名+参数）
     * @return true: 忽略，false: 需要处理
     */
    protected boolean isIgnoredFullMethodWithPrefixByFullMethod(String fullMethod) {
        for (String ignoreFullMethodPrefix : ignoreFullMethodPrefixSet) {
            if (fullMethod.startsWith(ignoreFullMethodPrefix)) {
                if (logger.isDebugEnabled()) {
                    logger.debug("忽略完整方法使用该前缀的方法 {} {}", ignoreFullMethodPrefix, fullMethod);
                }
                return true;
            }
        }
        return false;
    }

    /**
     * 根据前缀判断方法名是否需要忽略
     *
     * @param methodName 方法名
     * @return true: 忽略，false: 需要处理
     */
    protected boolean isIgnoredMethodWithPrefixByMethodName(String methodName) {
        for (String ignoreMethodPrefix : ignoreMethodPrefixSet) {
            if (methodName.startsWith(ignoreMethodPrefix)) {
                if (logger.isDebugEnabled()) {
                    logger.debug("忽略方法名使用该前缀的方法 {} {}", ignoreMethodPrefix, methodName);
                }
                return true;
            }
        }
        return false;
    }

    /**
     * 根据关键字判断类名是否需要忽略
     *
     * @param className 完整类名
     * @return true: 忽略，false: 需要处理
     */
    protected boolean isIgnoredClassWithKeywordByClass(String className) {
        for (String ignoreClassKeyword : ignoreClassKeywordSet) {
            if (className.contains(ignoreClassKeyword)) {
                if (logger.isDebugEnabled()) {
                    logger.debug("忽略类名包含该关键字的方法 {} {}", ignoreClassKeyword, className);
                }
                return true;
            }
        }
        return false;
    }

    /**
     * 根据前缀判断完整方法（类名+方法名+参数）是否需要包含
     *
     * @param fullMethod 完整方法（类名+方法名+参数）
     * @return true: 包含，false: 未指定需要包含
     */
    protected boolean isIncludeFullMethodWithPrefixByFullMethod(String fullMethod) {
        for (String includeFullMethodPrefix : includeFullMethodPrefixSet) {
            if (fullMethod.startsWith(includeFullMethodPrefix)) {
                if (logger.isDebugEnabled()) {
                    logger.debug("包含完整方法使用该前缀的方法 {} {}", includeFullMethodPrefix, fullMethod);
                }
                return true;
            }
        }
        return false;
    }

    /**
     * 判断当前找到的方法是否需要忽略
     *
     * @param callType
     * @param fullMethod
     * @return false: 不忽略 true: 忽略
     */
    protected boolean ignoreCurrentMethod(String callType, String fullMethod) {
        // 判断方法调用类型是否需要忽略
        if (isIgnoredCallType(callType)) {
            return true;
        }

        // 检查当前方法是否需要包含
        if (!isIncludeFullMethodWithPrefixByFullMethod(fullMethod)) {
            String className = JACGClassMethodUtil.getClassNameFromMethod(fullMethod);
            // 根据关键字判断类名是否需要忽略
            if (isIgnoredClassWithKeywordByClass(className)) {
                return true;
            }

            // 根据前缀判断完整方法（类名+方法名+参数）是否需要忽略
            if (isIgnoredFullMethodWithPrefixByFullMethod(fullMethod)) {
                return true;
            }

            String methodNameWithArgs = JACGClassMethodUtil.getMethodNameWithArgsFromFull(fullMethod);
            /*
                根据方法名前缀判断是否需要忽略，使用包含参数的方法名进行比较
                若当前调用类型为Runnable/Callable实现类子类构造函数调用run()方法，或其他类似情况，则不判断方法名前缀是否需要忽略（<init> -> run()，可能会被指定为忽略）
             */
            if (!JavaCG2CallTypeEnum.isInitMethodCallType(callType) && isIgnoredMethodWithPrefixByMethodName(methodNameWithArgs)) {
                return true;
            }
        }
        return false;
    }

    /**
     * 为方法调用信息增加是否在其他线程执行标志
     *
     * @param callInfo
     * @param methodCallId
     * @param callType
     * @param methodAnnotationMap
     */
    protected void addRunInOtherThread(StringBuilder callInfo, int methodCallId, String callType, Map<String, Map<String, BaseAnnotationAttribute>> methodAnnotationMap) {
        if (JavaCG2CallTypeEnum.isRunInOtherThreadType(callType)) {
            // 方法调用类型属于线程调用，在方法调用上增加在其他线程执行的标志
            doAddRunInOtherThread(callInfo);
            return;
        }

        if (methodAnnotationMap != null && methodAnnotationMap.get(JACGCommonNameConstants.SPRING_ASYNC_ANNOTATION) != null) {
            // 方法上的注解包括@Async，在方法调用上增加在其他线程执行的标志
            doAddRunInOtherThread(callInfo);
            return;
        }

        if (JavaCG2CallTypeEnum.CTE_LAMBDA.getType().equals(callType)) {
            WriteDbData4LambdaMethodInfo lambdaCalleeInfo = lambdaMethodHandler.queryLambdaCalleeInfo(methodCallId);
            if (lambdaCalleeInfo != null &&
                    ((JavaCG2CommonNameConstants.CLASS_NAME_RUNNABLE.equals(lambdaCalleeInfo.getLambdaCalleeClassName()) &&
                            JavaCG2CommonNameConstants.METHOD_RUNNABLE_RUN.equals(lambdaCalleeInfo.getLambdaCalleeMethodName()))
                            || (JavaCG2CommonNameConstants.CLASS_NAME_CALLABLE.equals(lambdaCalleeInfo.getLambdaCalleeClassName()) &&
                            JavaCG2CommonNameConstants.METHOD_CALLABLE_CALL.equals(lambdaCalleeInfo.getLambdaCalleeMethodName()))
                    )) {
                // 方法为Lambda表达式，且属于线程调用，在方法调用上增加在其他线程执行的标志
                doAddRunInOtherThread(callInfo);
            }
        }
    }

    // 在方法调用上增加在其他线程执行的标志
    private void doAddRunInOtherThread(StringBuilder callInfo) {
        callInfo.append(JACGConstants.CALL_FLAG_RUN_IN_OTHER_THREAD);
    }

    /**
     * 为方法调用信息增加是否在Spring事务中执行标志
     *
     * @param callInfo
     * @param methodCallId
     * @param callType
     * @param methodAnnotationMap
     */
    protected void addRunInSpringTransaction(StringBuilder callInfo, int methodCallId, String callType, Map<String, Map<String, BaseAnnotationAttribute>> methodAnnotationMap) {
        if (JavaCG2CallTypeEnum.isRunInSpringTxType(callType)) {
            // 方法调用类型属于事务调用，在方法调用上增加在事务中执行的标志
            doAddRunInSpringTransaction(callInfo);
            return;
        }

        if (methodAnnotationMap != null && methodAnnotationMap.get(JACGCommonNameConstants.SPRING_TX_ANNOTATION) != null) {
            // 方法上的注解包括@Transactional，在方法调用上增加在事务中执行的标志
            doAddRunInSpringTransaction(callInfo);
            return;
        }

        if (JavaCG2CallTypeEnum.CTE_LAMBDA.getType().equals(callType)) {
            WriteDbData4LambdaMethodInfo lambdaCalleeInfo = lambdaMethodHandler.queryLambdaCalleeInfo(methodCallId);
            if (lambdaCalleeInfo != null && (
                    (JavaCG2CommonNameConstants.CLASS_NAME_TRANSACTION_CALLBACK.equals(lambdaCalleeInfo.getLambdaCalleeClassName()) &&
                            JavaCG2CommonNameConstants.METHOD_DO_IN_TRANSACTION.equals(lambdaCalleeInfo.getLambdaCalleeMethodName()))
            )) {
                // 方法为Lambda表达式，且属于事务调用，在方法调用上增加在事务中执行的标志
                doAddRunInSpringTransaction(callInfo);
            }
        }
    }

    // 在方法调用上增加在Spring事务中执行的标志
    private void doAddRunInSpringTransaction(StringBuilder callInfo) {
        callInfo.append(JACGConstants.CALL_FLAG_RUN_IN_SPRING_TX);
    }

    /**
     * 添加方法调用业务功能数据
     *
     * @param methodCallId  方法调用序号
     * @param callFlags     方法调用标志
     * @param methodHash    对应的方法HASH+长度
     * @param callGraphInfo 调用信息
     * @return
     */
    protected boolean addBusinessData(int methodCallId,
                                      int callFlags,
                                      String methodHash,
                                      StringBuilder callGraphInfo) {
        // 添加默认的方法调用业务功能数据
        if (!addDefaultBusinessData(methodCallId, callFlags, methodHash, callGraphInfo)) {
            return false;
        }

        if (!MethodCallFlagsEnum.MCFE_EE_BUSINESS_DATA.checkFlag(callFlags)) {
            // 被调用方法不存在业务功能数据
            return true;
        }

        // 存在程序识别的方法调用业务功能数据，从数据库查询
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.BD_QUERY_BUSINESS_DATA;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.joinColumns(DC.BD_DATA_TYPE, DC.BD_DATA_VALUE) +
                    " from " + DbTableInfoEnum.DTIE_BUSINESS_DATA.getTableName() +
                    " where " + DC.BD_CALL_ID + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        BaseBusinessData businessData = dbOperator.queryObject(sql, BaseBusinessData.class, methodCallId);
        if (businessData == null) {
            logger.error("查询方法调用业务功能数据不存在 {}", methodCallId);
            return false;
        }

        // 将方法调用业务功能数据加入被调用方法信息中
        addBusinessData2CallGraphInfo(businessData.getDataType(), businessData.getDataValue(), callGraphInfo);
        return true;
    }

    // 添加默认的方法调用业务功能数据
    private boolean addDefaultBusinessData(int methodCallId,
                                           int callFlags,
                                           String methodHash,
                                           StringBuilder callGraphInfo) {
        for (String businessDataType : businessDataTypeList) {
            if (DefaultBusinessDataTypeEnum.BDTE_METHOD_CALL_INFO.getType().equals(businessDataType)) {
                // 显示方法调用信息
                if (!MethodCallFlagsEnum.MCFE_METHOD_CALL_INFO.checkFlag(callFlags)) {
                    continue;
                }

                ObjArgsInfoInMethodCall objArgsInfoInMethodCall = methodCallInfoHandler.queryObjArgsInfoInMethodCall(methodCallId);
                if (objArgsInfoInMethodCall == null) {
                    return false;
                }
                addBusinessData2CallGraphInfo(businessDataType, objArgsInfoInMethodCall, callGraphInfo);
            } else if (DefaultBusinessDataTypeEnum.BDTE_METHOD_ARG_GENERICS_TYPE.getType().equals(businessDataType)) {
                // 显示方法参数泛型类型
                if (!addMethodArgsGenericsTypeInfo(false, callFlags, methodHash, callGraphInfo)) {
                    return false;
                }
            } else if (DefaultBusinessDataTypeEnum.BDTE_METHOD_RETURN_GENERICS_TYPE.getType().equals(businessDataType)) {
                // 显示方法返回泛型类型
                if (!addMethodReturnGenericsTypeInfo(false, callFlags, methodHash, callGraphInfo)) {
                    return false;
                }
            }
        }

        return true;
    }

    /**
     * 显示方法参数泛型类型
     *
     * @param handleEntryMethod 是否处理入口方法
     * @param callFlags
     * @param methodHash
     * @param callGraphInfo
     * @return
     */
    protected boolean addMethodArgsGenericsTypeInfo(boolean handleEntryMethod, int callFlags, String methodHash, StringBuilder callGraphInfo) {
        if (handleEntryMethod) {
             /*
                处理入口方法
                生成向上的完整方法调用链时，判断被调用方法（即入口方法）是否存在参数泛型类型
                生成向下的完整方法调用链时，判断调用方法（即入口方法）是否存在参数泛型类型
             */
            if ((order4ee && !MethodCallFlagsEnum.MCFE_EE_ARGS_WITH_GENERICS_TYPE.checkFlag(callFlags)) ||
                    (!order4ee && !MethodCallFlagsEnum.MCFE_ER_ARGS_WITH_GENERICS_TYPE.checkFlag(callFlags))) {
                return true;
            }
        } else if ((order4ee && !MethodCallFlagsEnum.MCFE_ER_ARGS_WITH_GENERICS_TYPE.checkFlag(callFlags)) ||
                (!order4ee && !MethodCallFlagsEnum.MCFE_EE_ARGS_WITH_GENERICS_TYPE.checkFlag(callFlags))) {
             /*
                处理非入口方法
                生成向上的完整方法调用链时，判断调用方法是否存在参数泛型类型
                生成向下的完整方法调用链时，判断被调用方法是否存在参数泛型类型
             */
            return true;
        }

        MethodArgGenericsTypeInfo methodArgGenericsTypeInfo = methodArgReturnHandler.queryArgsGenericsTypeInfo(methodHash);
        if (methodArgGenericsTypeInfo == null) {
            logger.error("未查询到方法参数的集合泛型信息 {}", methodHash);
            return false;
        }
        addBusinessData2CallGraphInfo(DefaultBusinessDataTypeEnum.BDTE_METHOD_ARG_GENERICS_TYPE.getType(), methodArgGenericsTypeInfo, callGraphInfo);
        return true;
    }

    /**
     * 显示方法返回泛型类型
     *
     * @param handleEntryMethod 是否处理入口方法
     * @param callFlags
     * @param methodHash
     * @param callGraphInfo
     * @return
     */
    protected boolean addMethodReturnGenericsTypeInfo(boolean handleEntryMethod, int callFlags, String methodHash, StringBuilder callGraphInfo) {
        if (handleEntryMethod) {
             /*
                处理入口方法
                生成向上的完整方法调用链时，判断被调用方法（即入口方法）是否存在返回泛型类型
                生成向下的完整方法调用链时，判断调用方法（即入口方法）是否存在返回泛型类型
             */
            if ((order4ee && !MethodCallFlagsEnum.MCFE_EE_RETURN_WITH_GENERICS_TYPE.checkFlag(callFlags)) ||
                    (!order4ee && !MethodCallFlagsEnum.MCFE_ER_RETURN_WITH_GENERICS_TYPE.checkFlag(callFlags))) {
                return true;
            }
        } else if ((order4ee && !MethodCallFlagsEnum.MCFE_ER_RETURN_WITH_GENERICS_TYPE.checkFlag(callFlags)) ||
                (!order4ee && !MethodCallFlagsEnum.MCFE_EE_RETURN_WITH_GENERICS_TYPE.checkFlag(callFlags))) {
             /*
                处理非入口方法
                生成向上的完整方法调用链时，判断调用方法是否存在返回泛型类型
                生成向下的完整方法调用链时，判断被调用方法是否存在返回泛型类型
             */
            return true;
        }

        GenericsTypeValue methodReturnGenericsTypeInfo = methodArgReturnHandler.queryReturnGenericsTypeInfo(methodHash);
        if (methodReturnGenericsTypeInfo == null) {
            logger.error("未查询到方法返回的集合泛型信息 {}", methodHash);
            return false;
        }
        addBusinessData2CallGraphInfo(DefaultBusinessDataTypeEnum.BDTE_METHOD_RETURN_GENERICS_TYPE.getType(), methodReturnGenericsTypeInfo, callGraphInfo);
        return true;
    }

    /**
     * 将方法调用业务功能数据加入被调用方法信息中
     *
     * @param dataType      方法调用业务功能数据类型
     * @param dataValue     方法调用业务功能数据值
     * @param callGraphInfo 方法调用信息
     */
    protected void addBusinessData2CallGraphInfo(String dataType, Object dataValue, StringBuilder callGraphInfo) {
        String jsonStr = JACGJsonUtil.getJsonStr(dataValue);
        addBusinessData2CallGraphInfo(dataType, jsonStr, callGraphInfo);
    }

    /**
     * 将方法调用业务功能数据加入被调用方法信息中
     *
     * @param dataType      方法调用业务功能数据类型
     * @param dataValue     方法调用业务功能数据值
     * @param callGraphInfo 方法调用信息
     */
    protected void addBusinessData2CallGraphInfo(String dataType, String dataValue, StringBuilder callGraphInfo) {
        callGraphInfo.append(JavaCG2Constants.FLAG_TAB)
                .append(JACGConstants.CALL_FLAG_BUSINESS_DATA)
                .append(dataType)
                .append(JACGConstants.FLAG_AT)
                .append(dataValue);
    }
}
