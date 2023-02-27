package com.adrninistrator.jacg.runner.base;

import com.adrninistrator.jacg.annotation.formatter.AbstractAnnotationFormatter;
import com.adrninistrator.jacg.annotation.storage.AnnotationStorage;
import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.common.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.dto.annotation_attribute.BaseAnnotationAttribute;
import com.adrninistrator.jacg.dto.multiple.MultiCallInfo;
import com.adrninistrator.jacg.dto.notice.NoticeCallInfo;
import com.adrninistrator.jacg.dto.task.FindMethodTaskInfo;
import com.adrninistrator.jacg.markdown.enums.MDCodeBlockTypeEnum;
import com.adrninistrator.jacg.markdown.writer.MarkdownWriter;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Callee;
import com.adrninistrator.jacg.util.JACGCallGraphFileUtil;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGFileUtil;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.enums.CallTypeEnum;
import com.adrninistrator.javacg.util.JavaCGUtil;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
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

    // 配置文件中指定的需要处理的任务
    protected Set<String> taskSet;

    /*
        key: 配置文件中指定的类名
        value: 对应的简单类名或完整类名
     */
    protected Map<String, String> simpleClassNameMap = new HashMap<>();

    // 保存当前生成输出文件时的目录前缀
    protected String outputDirPrefix;

    // 完整方法（类名+方法名+参数）为以下前缀时，生成方法完整调用链时忽略
    protected Set<String> ignoreFullMethodPrefixSet;

    // 当类名包含以下关键字时，生成方法完整调用链时忽略
    protected Set<String> ignoreClassKeywordSet;

    // 当方法名为以下前缀时，生成方法完整调用链时忽略
    protected Set<String> ignoreMethodPrefixSet;

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
    protected Map<String, String> methodAllAnnotationInfoMap = new HashMap<>();

    // 保存已生成的过方法文件名
    protected Set<String> writtenFileNameSet = ConcurrentHashMap.newKeySet();

    // 保存类及方法上的注解信息
    protected AnnotationStorage annotationStorage;

    // 公共预处理
    protected boolean commonPreHandle() {
        // 从数据库查询数据需要在以上检查H2数据库文件之后
        if (!dbOperWrapper.findDuplicateClass()) {
            return false;
        }

        // 检查jar包文件是否有更新
        if (checkJarFileUpdated()) {
            return false;
        }

        // 获取生成方法完整调用链时需要忽略的信息
        if (!initIgnoreInfo()) {
            return false;
        }

        // 初始化保存类及方法上的注解信息
        initAnnotationStorage();

        // 添加用于添加对方法上的注解进行处理的类
        if (!addMethodAnnotationHandlerExtensions()) {
            return false;
        }

        return true;
    }

    // 初始化保存类及方法上的注解信息
    protected void initAnnotationStorage() {
        annotationStorage = new AnnotationStorage(dbOperator, dbOperWrapper);
    }

    // 获取生成方法完整调用链时需要忽略的信息
    protected boolean initIgnoreInfo() {
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
        return true;
    }

    /**
     * 获取唯一类名（简单类名或完整类名）
     *
     * @param className
     * @return null: 未获取到，非null: 若不存在同名类，则返回简单类名；若存在同名类，则返回完整类名
     */
    protected String getSimpleClassName(String className) {
        String simpleClassName = simpleClassNameMap.get(className);
        if (simpleClassName != null) {
            return simpleClassName;
        }

        // 执行获取唯一类名（简单类名或完整类名）
        simpleClassName = doGetSimpleClassName(className);
        if (simpleClassName == null) {
            return null;
        }

        simpleClassNameMap.putIfAbsent(className, simpleClassName);
        return simpleClassName;
    }

    // 执行获取唯一类名（简单类名或完整类名）
    protected String doGetSimpleClassName(String className) {
        if (className.contains(JavaCGConstants.FLAG_DOT)) {
            // 当前指定的是完整类名，查找对应的简单类名
            SqlKeyEnum sqlKeyEnum = SqlKeyEnum.CN_QUERY_SIMPLE_CLASS;
            String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
            if (sql == null) {
                sql = "select " + DC.CN_SIMPLE_CLASS_NAME +
                        " from " + DbTableInfoEnum.DTIE_CLASS_NAME.getTableName(dbOperWrapper.getAppName()) +
                        " where " + DC.CN_CLASS_NAME + " = ?";
                dbOperWrapper.cacheSql(sqlKeyEnum, sql);
            }

            List<Object> list = dbOperator.queryListOneColumn(sql, new Object[]{className});
            if (JACGUtil.isCollectionEmpty(list)) {
                logger.error("指定的完整类名 {} 不存在，请检查，可能因为指定的类所在的jar包未在配置文件 {}中指定",
                        className, OtherConfigFileUseListEnum.OCFULE_JAR_DIR.getKey());
                return null;
            }
            return (String) list.get(0);
        }

        // 当前指定的是简单类名
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.CN_QUERY_CLASS;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + DC.CN_SIMPLE_CLASS_NAME +
                    " from " + DbTableInfoEnum.DTIE_CLASS_NAME.getTableName(dbOperWrapper.getAppName()) +
                    " where " + DC.CN_SIMPLE_CLASS_NAME + " = ?";
            dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        List<Object> list = dbOperator.queryListOneColumn(sql, new Object[]{className});
        if (JACGUtil.isCollectionEmpty(list)) {
            logger.error("指定的简单类名 {} 不存在，请检查，可能因为以下原因\n" +
                            "1. 指定的类所在的jar包未在配置文件 {} 中指定\n" +
                            "2. 指定的类存在同名类，需要使用完整类名形式",
                    className, OtherConfigFileUseListEnum.OCFULE_JAR_DIR.getKey());
            return null;
        }
        return (String) list.get(0);
    }

    // 读取配置文件中指定的需要处理的任务
    protected boolean readTaskInfo(OtherConfigFileUseSetEnum otherConfigFileUseSetEnum) {
        taskSet = configureWrapper.getOtherConfigSet(otherConfigFileUseSetEnum, true);
        if (JACGUtil.isCollectionEmpty(taskSet)) {
            logger.error("读取文件不存在或内容为空 {}", otherConfigFileUseSetEnum.getKey());
            return false;
        }

        return true;
    }

    /**
     * 创建输出文件所在目录
     * 需要进行同步控制，避免创建同名目录
     *
     * @param prefix
     * @return
     */
    protected boolean createOutputDir(String prefix) {
        synchronized (AbstractRunnerGenCallGraph.class) {
            String tmpOutputDirPrefix;
            String outputRootPathInProperties = confInfo.getOutputRootPath();
            if (StringUtils.isNotBlank(outputRootPathInProperties)) {
                if (!StringUtils.endsWithAny(outputRootPathInProperties, "/", "\\")) {
                    // 若指定的目录未以分隔符结尾，则增加
                    outputRootPathInProperties += File.separator;
                }
                // 使用指定的生成结果文件根目录，并指定当前应用名称
                tmpOutputDirPrefix = outputRootPathInProperties + prefix + File.separator + dbOperWrapper.getAppName() + JACGConstants.FLAG_MINUS + JavaCGUtil.currentTime();
            } else {
                // 使用当前目录作为生成结果文件根目录
                tmpOutputDirPrefix = prefix + File.separator + JavaCGUtil.currentTime();
            }

            outputDirPrefix = new File(tmpOutputDirPrefix).getAbsolutePath();

            logger.info("创建保存输出文件的目录 {}", outputDirPrefix);
            // 判断目录是否存在，不存在时尝试创建
            if (!JACGFileUtil.isDirectoryExists(outputDirPrefix)) {
                return false;
            }
        }

        // 打印当前使用的配置信息
        printUsedConfigInfo(outputDirPrefix);
        return true;
    }

    // 根据调用关系ID获取用于提示的信息
    private NoticeCallInfo queryNoticeCallInfo(int currentMethodCallId) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MC_QUERY_NOTICE_INFO;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.joinColumns(DC.MC_CALLER_METHOD_HASH, DC.MC_CALLER_FULL_METHOD, DC.MC_CALLEE_FULL_METHOD) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName(dbOperWrapper.getAppName()) +
                    " where " + DC.MC_CALL_ID + " = ?";
            dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        Map<String, Object> map = dbOperator.queryOneRow(sql, new Object[]{currentMethodCallId});
        if (JACGUtil.isMapEmpty(map)) {
            logger.error("查询需要提示的信息失败 {}", currentMethodCallId);
            return null;
        }

        String callerMethodHash = (String) map.get(DC.MC_CALLER_METHOD_HASH);
        String callerFullMethod = (String) map.get(DC.MC_CALLER_FULL_METHOD);
        String calleeFullMethod = (String) map.get(DC.MC_CALLEE_FULL_METHOD);
        if (StringUtils.isAnyBlank(callerMethodHash, callerFullMethod, calleeFullMethod)) {
            logger.error("查询需要提示的信息存在空值 {}", currentMethodCallId);
            return null;
        }
        return new NoticeCallInfo(callerMethodHash, callerFullMethod, calleeFullMethod);
    }

    // 记录可能出现一对多的方法调用
    protected boolean recordMethodCallMayBeMulti(int currentMethodCallId, String callType) {
        CallTypeEnum callTypeEnum = CallTypeEnum.getFromType(callType);
        if (callTypeEnum != CallTypeEnum.CTE_INTERFACE_CALL_IMPL_CLASS && callTypeEnum != CallTypeEnum.CTE_SUPER_CALL_CHILD) {
            // 对于接口调用实现类、父类调用子类之外的情况，不判断是否出现出现一对多的方法调用
            return true;
        }

        // 对于接口调用实现类、父类调用子类，判断是否出现出现一对多的方法调用
        // 根据调用关系ID获取用于提示的信息
        NoticeCallInfo noticeCallInfo = queryNoticeCallInfo(currentMethodCallId);
        if (noticeCallInfo == null) {
            return false;
        }

        Map<String, MultiCallInfo> methodCallMap;
        Set<String> multiCallerFullMethodSet;

        if (callTypeEnum == CallTypeEnum.CTE_INTERFACE_CALL_IMPL_CLASS) {
            methodCallMap = itfMethodCallMap;
            multiCallerFullMethodSet = itfMultiCallerFullMethodSet;
        } else {
            methodCallMap = sccMethodCallMap;
            multiCallerFullMethodSet = sccMultiCallerFullMethodSet;
        }

        String callerMethodHash = noticeCallInfo.getCallerMethodHash();
        String callerFullMethod = noticeCallInfo.getCallerFullMethod();
        String calleeFullMethod = noticeCallInfo.getCalleeFullMethod();

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
        CallTypeEnum callTypeEnum = CallTypeEnum.getFromType(callType);
        if (callTypeEnum != CallTypeEnum.CTE_INTERFACE_CALL_IMPL_CLASS && callTypeEnum != CallTypeEnum.CTE_SUPER_CALL_CHILD) {
            return true;
        }

        // 根据调用关系ID获取用于提示的信息
        NoticeCallInfo noticeCallInfo = queryNoticeCallInfo(callId);
        if (noticeCallInfo == null) {
            return false;
        }

        Map<String, MultiCallInfo> methodCallMap;

        if (callTypeEnum == CallTypeEnum.CTE_INTERFACE_CALL_IMPL_CLASS) {
            methodCallMap = disabledItfMethodCallMap;
        } else {
            methodCallMap = disabledSccMethodCallMap;
        }

        String callerMethodHash = noticeCallInfo.getCallerMethodHash();
        String callerFullMethod = noticeCallInfo.getCallerFullMethod();
        String calleeFullMethod = noticeCallInfo.getCalleeFullMethod();

        MultiCallInfo multiCallInfo = methodCallMap.computeIfAbsent(callerFullMethod, k -> new MultiCallInfo(callerMethodHash, ConcurrentHashMap.newKeySet()));
        multiCallInfo.getCalleeFullMethodSet().add(calleeFullMethod);
        return true;
    }

    // 打印存在一对多的方法调用
    private void printMultiMethodCall(Map<String, MultiCallInfo> methodCallMap, Set<String> multiCallerFullMethodSet, CallTypeEnum callTypeEnum) {
        // 判断相关存在一对多的调用者方法是否有被其他方法调用，若未被调用则不显示
        List<String> multiCallerFullMethodList = new ArrayList<>(multiCallerFullMethodSet.size());
        for (String multiCallerFullMethod : multiCallerFullMethodSet) {
            String multiCallerMethodHash = JACGUtil.genHashWithLen(multiCallerFullMethod);
            if (Boolean.TRUE.equals(dbOperWrapper.checkExistsNormalMethodCallByCalleeMethodHash(multiCallerMethodHash))) {
                // 当前存在一对多的调用者方法有被其他方法调用
                multiCallerFullMethodList.add(multiCallerFullMethod);
            } else {
                logger.warn("当前存在一对多的调用者方法未被其他方法调用，不打印到文件中 {}", multiCallerFullMethod);
            }
        }

        if (multiCallerFullMethodList.isEmpty()) {
            logger.info("{} 不存在一对多的方法调用，不打印相关信息", callTypeEnum);
            return;
        }

        String filePath;
        if (CallTypeEnum.CTE_INTERFACE_CALL_IMPL_CLASS == callTypeEnum) {
            filePath = outputDirPrefix + File.separator + JACGConstants.NOTICE_MULTI_ITF_MD;
        } else {
            filePath = outputDirPrefix + File.separator + JACGConstants.NOTICE_MULTI_SCC_MD;
        }

        logger.info("{} 存在一对多的方法调用，打印相关信息 {}", callTypeEnum, filePath);
        try (MarkdownWriter markdownWriter = new MarkdownWriter(filePath, true)) {
            markdownWriter.addTitle(1, "说明");

            if (CallTypeEnum.CTE_INTERFACE_CALL_IMPL_CLASS == callTypeEnum) {
                markdownWriter.addLineWithNewLine("出现当前文件，说明接口调用对应实现类的方法调用存在一对多的方法调用");
            } else {
                markdownWriter.addLineWithNewLine("出现当前文件，说明抽象父类调用对应子类的方法调用存在一对多的方法调用");
            }

            markdownWriter.addLineWithNewLine("可以使用以下SQL语句查找对应的方法调用并禁用，仅保留需要的调用关系");
            markdownWriter.addCodeBlock(MDCodeBlockTypeEnum.MDCBTE_SQL);
            // 生成提示信息中的查询SQL
            markdownWriter.addLine(genNoticeSelectSql(callTypeEnum.getType()));
            // 生成提示信息中的更新为禁用SQL
            markdownWriter.addLine(genNoticeUpdateDisableSql(callTypeEnum.getType()));

            if (this instanceof RunnerGenAllGraph4Callee) {
                // 生成向上的方法调用完整调用链时，增加一个显示的update语句
                markdownWriter.addEmptyLine();
                markdownWriter.addLine(genNoticeSelectSql4Callee());
                markdownWriter.addLine(genNoticeUpdateDisableSql4Callee());
            }
            markdownWriter.addCodeBlock();

            Collections.sort(multiCallerFullMethodList);
            for (String multiCallerFullMethod : multiCallerFullMethodList) {
                MultiCallInfo multiCallInfo = methodCallMap.get(multiCallerFullMethod);
                if (multiCallInfo == null || JACGUtil.isCollectionEmpty(multiCallInfo.getCalleeFullMethodSet())) {
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
    private void printDisabledMethodCall(Map<String, MultiCallInfo> disabledMethodCallMap, CallTypeEnum callTypeEnum) {
        if (disabledMethodCallMap.isEmpty()) {
            logger.info("{} 不存在被禁用的方法调用，不打印相关信息", callTypeEnum);
            return;
        }

        String filePath;
        if (CallTypeEnum.CTE_INTERFACE_CALL_IMPL_CLASS == callTypeEnum) {
            filePath = outputDirPrefix + File.separator + JACGConstants.NOTICE_DISABLED_ITF_MD;
        } else {
            filePath = outputDirPrefix + File.separator + JACGConstants.NOTICE_DISABLED_SCC_MD;
        }

        logger.info("{} 存在被禁用的方法调用，打印相关信息 {}", callTypeEnum, filePath);

        try (MarkdownWriter markdownWriter = new MarkdownWriter(filePath, true)) {
            markdownWriter.addTitle(1, "说明");

            if (CallTypeEnum.CTE_INTERFACE_CALL_IMPL_CLASS == callTypeEnum) {
                markdownWriter.addLineWithNewLine("出现当前文件，说明接口调用对应实现类的方法调用存在被禁用的方法调用");
            } else {
                markdownWriter.addLineWithNewLine("出现当前文件，说明抽象父类调用对应子类的方法调用存在被禁用的方法调用");
            }
            markdownWriter.addLineWithNewLine("可以使用以下SQL语句查找对应的方法调用并启用");
            markdownWriter.addCodeBlock(MDCodeBlockTypeEnum.MDCBTE_SQL);

            // 生成提示信息中的查询SQL
            markdownWriter.addLineWithNewLine(genNoticeSelectSql(callTypeEnum.getType()));
            // 生成提示信息中的更新为禁用SQL
            markdownWriter.addLine(genNoticeUpdateEnableSql(callTypeEnum.getType()));
            markdownWriter.addCodeBlock();

            List<String> disabledCallerMethodList = new ArrayList<>(disabledMethodCallMap.keySet());
            Collections.sort(disabledCallerMethodList);
            for (String disabledCallerMethod : disabledCallerMethodList) {
                MultiCallInfo multiCallInfo = disabledMethodCallMap.get(disabledCallerMethod);
                if (multiCallInfo == null || JACGUtil.isCollectionEmpty(multiCallInfo.getCalleeFullMethodSet())) {
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
        printMultiMethodCall(itfMethodCallMap, itfMultiCallerFullMethodSet, CallTypeEnum.CTE_INTERFACE_CALL_IMPL_CLASS);
        printMultiMethodCall(sccMethodCallMap, sccMultiCallerFullMethodSet, CallTypeEnum.CTE_SUPER_CALL_CHILD);
        printDisabledMethodCall(disabledItfMethodCallMap, CallTypeEnum.CTE_INTERFACE_CALL_IMPL_CLASS);
        printDisabledMethodCall(disabledSccMethodCallMap, CallTypeEnum.CTE_SUPER_CALL_CHILD);
    }

    // 生成提示信息中的查询SQL
    private String genNoticeSelectSql(String callType) {
        return "select * from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName(dbOperWrapper.getAppName()) +
                " where " + DC.MC_CALL_TYPE + " = '" + callType +
                "' and " + DC.MC_CALLER_METHOD_HASH + " = '';";
    }

    private String genNoticeSelectSql4Callee() {
        return "select * from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName(dbOperWrapper.getAppName()) +
                " where " + DC.MC_CALLEE_METHOD_HASH + " = '';";
    }

    // 生成提示信息中的更新为禁用SQL
    private String genNoticeUpdateDisableSql(String callType) {
        return "update " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName(dbOperWrapper.getAppName()) +
                " set " + DC.MC_ENABLED + " = " + JACGConstants.NO_0 +
                " where " + DC.MC_CALL_TYPE + " = '" + callType +
                "' and " + DC.MC_CALLER_METHOD_HASH + " = '' and " + DC.MC_CALLEE_FULL_METHOD + " <> '';";
    }

    private String genNoticeUpdateDisableSql4Callee() {
        return "update " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName(dbOperWrapper.getAppName()) +
                " set " + DC.MC_ENABLED + " = " + JACGConstants.NO_0 +
                " where " + DC.MC_CALLEE_METHOD_HASH + " = '' and " + DC.MC_CALLER_FULL_METHOD + " <> '';";
    }

    // 生成提示信息中的更新为启用SQL
    private String genNoticeUpdateEnableSql(String callType) {
        return "update " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName(dbOperWrapper.getAppName()) +
                " set " + DC.MC_ENABLED + " = " + JACGConstants.YES_1 +
                " where " + DC.MC_CALL_TYPE + " = '" + callType +
                "' and " + DC.MC_CALLER_METHOD_HASH + " = '' and " + DC.MC_CALLEE_FULL_METHOD + " = '';";
    }

    /**
     * 检查jar包文件是否有更新
     *
     * @return true: 有更新，false: 没有更新
     */
    protected boolean checkJarFileUpdated() {
        if (!confInfo.isCheckJarFileUpdated()) {
            logger.info("不检查jar包文件是否有更新");
            return false;
        }

        Map<String, Map<String, Object>> jarInfoMap = queryJarFileInfo();
        if (JACGUtil.isMapEmpty(jarInfoMap)) {
            return false;
        }

        for (String jarPath : getJarPathList()) {
            if (!JACGFileUtil.isFileExists(jarPath)) {
                continue;
            }

            String jarFilePath = JACGFileUtil.getCanonicalPath(jarPath);
            if (jarFilePath == null) {
                logger.error("获取文件路径失败: {}", jarPath);
                return true;
            }

            String jarPathHash = JACGUtil.genHashWithLen(jarFilePath);
            Map<String, Object> jarInfo = jarInfoMap.get(jarPathHash);
            if (jarInfo == null) {
                String jarFullPath = jarPath.equals(jarFilePath) ? "" : jarFilePath;
                logger.error("指定的jar包未导入数据库中，请先执行 TestRunnerWriteDb 类导入数据库\n{} {}", jarPath, jarFullPath);
                return true;
            }

            long lastModified = JACGFileUtil.getFileLastModified(jarFilePath);
            String lastModifiedStr = String.valueOf(lastModified);
            if (!lastModifiedStr.equals(jarInfo.get(DC.JI_LAST_MODIFIED))) {
                String jarFileHash = JACGFileUtil.getFileMd5(jarFilePath);
                if (!StringUtils.equals(jarFileHash, (String) jarInfo.get(DC.JI_JAR_HASH))) {
                    String jarFullPath = jarPath.equals(jarFilePath) ? "" : jarFilePath;
                    logger.error("指定的jar包文件内容有变化，请先执行 TestRunnerWriteDb 类导入数据库\n{} {} {}\n" +
                                    "假如不需要检查jar包文件是否有更新，可修改配置文件参数值 {} 为 {}",
                            new Date(lastModified), jarPath, jarFullPath, ConfigKeyEnum.CKE_CHECK_JAR_FILE_UPDATED.getKey(), Boolean.FALSE);
                    return true;
                }
            }
        }

        return false;
    }

    @Override
    protected boolean checkH2DbFile() {
        File h2DbFile = getH2DbFile();
        if (!h2DbFile.exists()) {
            logger.error("H2数据库文件不存在，请先执行 TestRunnerWriteDb 类导入数据库 {}", JACGFileUtil.getCanonicalPath(h2DbFile));
            return false;
        }

        // 数据库文件存在
        if (!h2DbFile.isFile()) {
            logger.error("H2数据库文件不是文件 {}", JACGFileUtil.getCanonicalPath(h2DbFile));
            return false;
        }

        // 检查H2数据库文件是否可写
        return checkH2DbFileWritable(h2DbFile);
    }

    private Map<String, Map<String, Object>> queryJarFileInfo() {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.JI_QUERY_JAR_INFO;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select * from " + DbTableInfoEnum.DTIE_JAR_INFO.getTableName(dbOperWrapper.getAppName());
            dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        List<Map<String, Object>> list = dbOperator.queryList(sql, new Object[]{});
        if (JACGUtil.isCollectionEmpty(list)) {
            logger.error("查询到jar包信息为空");
            return null;
        }

        Map<String, Map<String, Object>> rtnMap = new HashMap<>(list.size());

        for (Map<String, Object> map : list) {
            String jarPathHash = (String) map.get(DC.JI_JAR_PATH_HASH);
            rtnMap.putIfAbsent(jarPathHash, map);
        }

        return rtnMap;
    }

    /**
     * 获取本次执行时的输出目录
     *
     * @return null: 执行失败，非null: 执行成功
     */
    public String getSuccessOutputDir() {
        if (someTaskFail) {
            return null;
        }
        return outputDirPrefix;
    }

    /**
     * 重新设置线程数，若当前线程数需要调大则修改
     *
     * @param taskNum 任务数量
     */
    protected void resetPoolSize(int taskNum) {
        int currentPoolSize = confInfo.getThreadNum();
        int newPoolSize = Math.min(confInfo.getOriginalThreadNum(), taskNum);

        if (currentPoolSize >= newPoolSize) {
            // 当前线程数比准备修改的线程数大，不处理
            return;
        }

        confInfo.setThreadNum(newPoolSize);
        // 先修改最大线程数，再修改核心线程数，避免内部处理报错
        threadPoolExecutor.setMaximumPoolSize(newPoolSize);
        threadPoolExecutor.setCorePoolSize(newPoolSize);

        dbOperator.setMaxPoolSize(newPoolSize);
    }

    // 添加用于添加对方法上的注解进行处理的类
    protected boolean addMethodAnnotationHandlerExtensions() {
        List<String> methodAnnotationHandlerClassList = configureWrapper.getOtherConfigList(OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_METHOD_ANNOTATION_HANDLER, true);
        if (JACGUtil.isCollectionEmpty(methodAnnotationHandlerClassList)) {
            return true;
        }

        annotationFormatterList = new ArrayList<>(methodAnnotationHandlerClassList.size());
        try {
            for (String extensionClass : methodAnnotationHandlerClassList) {
                AbstractAnnotationFormatter annotationFormatter = JACGUtil.getClassObject(extensionClass, AbstractAnnotationFormatter.class);
                if (annotationFormatter == null) {
                    return false;
                }
                annotationFormatter.setAnnotationStorage(annotationStorage);
                annotationFormatterList.add(annotationFormatter);
            }
            return true;
        } catch (Exception e) {
            logger.error("error ", e);
            return false;
        }
    }

    /**
     * 获取方法对应的注解信息
     *
     * @param methodHash 完整方法HASH+长度
     * @return
     */
    protected String getMethodAnnotationInfo(String fullMethod, String methodHash) {
        // 根据完整方法HASH+长度获取对应的注解信息
        Map<String, Map<String, BaseAnnotationAttribute>> methodAnnotationMap = annotationStorage.getAnnotationMap4FullMethod(fullMethod);
        if (methodAnnotationMap == null) {
            // 当前方法上没有注解
            return "";
        }

        // 当前方法上有注解
        String existedAnnotationInfo = methodAllAnnotationInfoMap.get(methodHash);
        if (existedAnnotationInfo != null) {
            // 当前方法对应的注解信息已查询过，直接使用
            return existedAnnotationInfo;
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
        return allAnnotationInfo;
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
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MLN_QUERY_METHOD;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.joinColumns(DC.MLN_METHOD_HASH, DC.MLN_FULL_METHOD) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_LINE_NUMBER.getTableName(dbOperWrapper.getAppName()) +
                    " where " + DC.MLN_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.MLN_MIN_LINE_NUMBER + " <= ?" +
                    " and " + DC.MLN_MAX_LINE_NUMBER + " >= ?" +
                    " limit 1";
            dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        Map<String, Object> map = dbOperator.queryOneRow(sql, new Object[]{simpleClassName, methodLineNum, methodLineNum});
        if (map == null) {
            return FindMethodTaskInfo.genFindMethodInfoFail();
        }

        if (JACGUtil.isMapEmpty(map)) {
            logger.warn("指定类的代码行号未查找到对应方法，请检查，可能因为以下原因\n" +
                    "1. 指定的类所在的jar包未在配置文件 {} 中指定\n" +
                    "2. 指定的方法是接口中未实现的方法\n" +
                    "3. 指定的方法是抽象方法\n" +
                    "{} {}", OtherConfigFileUseListEnum.OCFULE_JAR_DIR.getKey(), simpleClassName, methodLineNum);
            return FindMethodTaskInfo.genFindMethodInfoGenEmptyFile();
        }

        String methodHash = (String) map.get(DC.MLN_METHOD_HASH);
        // 查询方法的标记
        int methodCallFlags = queryMethodCallFlags(isCallee, methodHash);
        // 指定类的代码行号查找到对应方法
        return FindMethodTaskInfo.genFindMethodInfoSuccess(methodHash, (String) map.get(DC.MLN_FULL_METHOD), methodCallFlags);
    }

    /**
     * 查询方法的标记
     *
     * @param isCallee   true: 生成向上的方法调用链 false: 生成向下的方法调用链
     * @param methodHash
     * @return
     */
    protected int queryMethodCallFlags(boolean isCallee, String methodHash) {
        SqlKeyEnum sqlKeyEnum = isCallee ? SqlKeyEnum.MC_QUERY_FLAG_4EE : SqlKeyEnum.MC_QUERY_FLAG_4ER;
        String whereColumnName = isCallee ? DC.MC_CALLEE_METHOD_HASH : DC.MC_CALLER_METHOD_HASH;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + DC.MC_CALL_FLAGS +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName(dbOperWrapper.getAppName()) +
                    " where " + whereColumnName + " = ?" +
                    " limit 1";
            dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        Map<String, Object> map = dbOperator.queryOneRow(sql, new Object[]{methodHash});
        if (JACGUtil.isMapEmpty(map)) {
            return 0;
        }

        return (int) map.get(DC.MC_CALL_FLAGS);
    }

    /**
     * 当完整方法（类名+方法名+参数）为以下前缀时，忽略
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
     * 当方法名为以下前缀时，忽略
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
     * 当类名包含以下关键字时，忽略
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
     * 判断当前找到的方法是否需要忽略
     *
     * @param callType
     * @param fullMethod
     * @return false: 不忽略 true: 忽略
     */
    protected boolean ignoreCurrentMethod(String callType, String fullMethod) {
        // 当完整方法（类名+方法名+参数）为以下前缀时，忽略
        if (isIgnoredFullMethodWithPrefixByFullMethod(fullMethod)) {
            return true;
        }

        String className = JACGClassMethodUtil.getClassNameFromMethod(fullMethod);
        // 根据类名特定关键字判断是否需要忽略
        if (isIgnoredClassWithKeywordByClass(className)) {
            return true;
        }

        String methodNameWithArgs = JACGClassMethodUtil.getMethodNameWithArgsFromFull(fullMethod);
        /*
            根据方法名前缀判断是否需要忽略，使用包含参数的方法名进行比较
            若当前调用类型为Runnable/Callable实现类子类构造函数调用run()方法，或其他类似情况，则不判断方法名前缀是否需要忽略（<init> -> run()，可能会被指定为忽略）
         */
        if (!StringUtils.equalsAny(callType,
                CallTypeEnum.CTE_RUNNABLE_INIT_RUN.getType(),
                CallTypeEnum.CTE_CALLABLE_INIT_CALL.getType(),
                CallTypeEnum.CTE_TX_CALLBACK_INIT_CALL.getType(),
                CallTypeEnum.CTE_TX_CALLBACK_WR_INIT_CALL.getType()
        ) && isIgnoredMethodWithPrefixByMethodName(methodNameWithArgs)) {
            return true;
        }
        return false;
    }
}
