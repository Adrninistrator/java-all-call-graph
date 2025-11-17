package com.adrninistrator.jacg.runner.base;

import com.adrninistrator.jacg.annotation.formatter.AbstractAnnotationFormatter;
import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.JACGCommonNameConstants;
import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.DefaultBusinessDataTypeEnum;
import com.adrninistrator.jacg.common.enums.MethodCallFlagsEnum;
import com.adrninistrator.jacg.common.enums.OutputDetailEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.comparator.Comparator4FullMethodWithReturnType;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.conf.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.dto.annotation.BaseAnnotationAttribute;
import com.adrninistrator.jacg.dto.method.FullMethodWithReturnType;
import com.adrninistrator.jacg.dto.methodcall.MethodCallLineData;
import com.adrninistrator.jacg.dto.methodcall.ObjArgsInfoInMethodCall;
import com.adrninistrator.jacg.dto.multiple.MultiCallInfo;
import com.adrninistrator.jacg.dto.task.FindMethodTaskInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4LambdaMethodInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCall;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodLineNumber;
import com.adrninistrator.jacg.el.enums.ElConfigEnum;
import com.adrninistrator.jacg.el.manager.ElManager;
import com.adrninistrator.jacg.handler.annotation.AnnotationHandler;
import com.adrninistrator.jacg.handler.dto.businessdata.BaseBusinessData;
import com.adrninistrator.jacg.handler.dto.genericstype.GenericsTypeValue;
import com.adrninistrator.jacg.handler.dto.genericstype.MethodArgGenericsTypeInfo;
import com.adrninistrator.jacg.handler.lambda.LambdaMethodHandler;
import com.adrninistrator.jacg.handler.method.MethodArgReturnHandler;
import com.adrninistrator.jacg.handler.method.MethodInfoHandler;
import com.adrninistrator.jacg.handler.methodcall.MethodCallHandler;
import com.adrninistrator.jacg.handler.methodcall.MethodCallInfoHandler;
import com.adrninistrator.jacg.handler.mybatis.MyBatisMSMapperEntityHandler;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Callee;
import com.adrninistrator.jacg.util.JACGCallGraphFileUtil;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGFileUtil;
import com.adrninistrator.jacg.util.JACGJsonUtil;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.javacg2.common.JavaCG2CommonNameConstants;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.common.enums.JavaCG2CallTypeEnum;
import com.adrninistrator.javacg2.common.enums.JavaCG2YesNoEnum;
import com.adrninistrator.javacg2.conf.enums.JavaCG2OtherConfigFileUseListEnum;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;
import com.adrninistrator.javacg2.markdown.enums.MDCodeBlockTypeEnum;
import com.adrninistrator.javacg2.markdown.writer.MarkdownWriter;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedWriter;
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
 * @description: 生成指定方法完整调用链，抽象父类
 */

public abstract class AbstractRunnerGenAllCallGraph extends AbstractRunner {
    private static final Logger logger = LoggerFactory.getLogger(AbstractRunnerGenAllCallGraph.class);

    // 当前生成的方法完整调用链方向是否为向上
    protected final boolean order4ee = this instanceof RunnerGenAllGraph4Callee;

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
    private final Set<FullMethodWithReturnType> itfMultiCallerMethodSet = ConcurrentHashMap.newKeySet();

    // 抽象父类调用对应子类的方法调用，存在一对多的抽象父类
    private final Set<FullMethodWithReturnType> sccMultiCallerMethodSet = ConcurrentHashMap.newKeySet();

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

    // 生成向上/向下的方法完整调用链时，生成的方法调用数量超过限制的方法
    protected final List<String> genCallGraphNumExceedMethodList = new ArrayList<>();

    // 是否将生成的调用链数据写入文件的开关
    protected boolean callGraphWriteToFile = true;

    // 是否将生成的调用链数据在内存中返回的开关
    protected boolean callGraphReturnInMemory = false;

    // 生成调用链时，文件名是否使用更短的模式
    protected boolean callGraphFileShortMode = false;

    // 配置文件中指定的需要处理的任务
    protected Set<String> taskSet;

    // 表达式管理类
    protected ElManager elManager;

    // 需要显示的业务功能数据类型，Set格式
    protected Set<String> businessDataTypeSet;

    // 需要显示的业务功能数据类型，List格式
    protected List<String> businessDataTypeList;

    protected AnnotationHandler annotationHandler;
    protected MethodInfoHandler methodInfoHandler;
    protected MethodCallHandler methodCallHandler;
    protected MethodCallInfoHandler methodCallInfoHandler;
    protected MethodArgReturnHandler methodArgReturnHandler;
    protected MyBatisMSMapperEntityHandler myBatisMSMapperEntityHandler;
    protected LambdaMethodHandler lambdaMethodHandler;

    // 保存用于对方法上的注解进行处理的类
    protected List<AbstractAnnotationFormatter> annotationFormatterList;

    // 保存各个方法已处理过的所有注解信息
    protected Map<String, String> methodAllAnnotationStrMap = new ConcurrentHashMap<>();

    // 保存已生成的过方法文件名
    protected Set<String> writtenFileNameSet = ConcurrentHashMap.newKeySet();

    // 输出结果展示详细程度枚举
    protected OutputDetailEnum outputDetailEnum;

    // 生成向上/向下的方法完整调用链时，每个方法允许生成的方法调用数量限制，默认不限制
    protected int genCallGraphNumLimit;

    // 生成向上/向下的方法完整调用链时，允许生成的方法完整调用链深度限制，默认不限制
    protected int genCallGraphDepthLimit;

    public AbstractRunnerGenAllCallGraph() {
        super();
    }

    public AbstractRunnerGenAllCallGraph(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
    }

    /**
     * 根据方法完整调用链每行的数据生成对应字符串，调用链不写入文件时也需要调用，否则内存中的相关数据也不会被写入
     *
     * @param methodCallLineData 方法完整调用链当前行的数据
     * @return
     */
    protected abstract String genMethodCallLineStr(MethodCallLineData methodCallLineData);

    protected boolean handleDb() {
        return true;
    }

    // 公共预处理
    protected boolean commonPreHandle() {
        callGraphWriteToFile = configureWrapper.getMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_WRITE_TO_FILE);
        callGraphReturnInMemory = configureWrapper.getMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_RETURN_IN_MEMORY);
        if (!callGraphWriteToFile && !callGraphReturnInMemory) {
            logger.error("是否将生成的调用链数据写入文件的开关，与是否将生成的调用链数据在内存中返回的开关，不允许都设置为true");
            return false;
        }

        callGraphFileShortMode = configureWrapper.getMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_FILE_SHORT_MODE);
        outputDetailEnum = OutputDetailEnum.getFromDetail(configureWrapper.getMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_OUTPUT_DETAIL));

        // 从数据库查询数据需要在以上检查H2数据库文件之后
        if (!dbOperWrapper.findDuplicateClass()) {
            return false;
        }

        if (!useNeo4j()) {
            // 初始相关处理类
            annotationHandler = new AnnotationHandler(dbOperWrapper);
            methodInfoHandler = new MethodInfoHandler(dbOperWrapper);
            methodCallHandler = new MethodCallHandler(dbOperWrapper);
            lambdaMethodHandler = new LambdaMethodHandler(dbOperWrapper);

            // 添加用于添加对方法上的注解进行处理的类
            if (!addMethodAnnotationHandlerExtensions()) {
                return false;
            }

            // 初始化默认的处理业务功能数据的类
            if (!initDefaultBusinessDataHandler()) {
                return false;
            }
        }

        genCallGraphNumLimit = configureWrapper.getMainConfig(ConfigKeyEnum.CKE_GEN_CALL_GRAPH_NUM_LIMIT);
        genCallGraphDepthLimit = configureWrapper.getMainConfig(ConfigKeyEnum.CKE_GEN_CALL_GRAPH_DEPTH_LIMIT);

        // todo
        methodCallInfoHandler = new MethodCallInfoHandler(dbOperWrapper);
        return true;
    }

    // 创建表达式管理类，需要在currentOutputDirPath赋值后执行
    protected void createElManager() {
        elManager = new ElManager(configureWrapper, ElConfigEnum.values(), currentOutputDirPath);
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

    // 根据调用关系ID获取用于提示的信息
    private WriteDbData4MethodCall queryNoticeCallInfo(int currentMethodCallId) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MC_QUERY_NOTICE_INFO;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.joinColumns(DC.MC_CALLER_METHOD_HASH, DC.MC_CALLER_FULL_METHOD, DC.MC_CALLER_RETURN_TYPE, DC.MC_CALLEE_FULL_METHOD,
                    DC.MC_RAW_RETURN_TYPE) +
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
        if (JavaCG2CallTypeEnum.CTE_INTERFACE_CALL_IMPL_CLASS != callTypeEnum && JavaCG2CallTypeEnum.CTE_SUPER_CALL_CHILD != callTypeEnum) {
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
        Set<FullMethodWithReturnType> multiCallerFullMethodSet;

        if (callTypeEnum == JavaCG2CallTypeEnum.CTE_INTERFACE_CALL_IMPL_CLASS) {
            methodCallMap = itfMethodCallMap;
            multiCallerFullMethodSet = itfMultiCallerMethodSet;
        } else {
            methodCallMap = sccMethodCallMap;
            multiCallerFullMethodSet = sccMultiCallerMethodSet;
        }

        String callerMethodHash = methodCall.getCallerMethodHash();
        String callerFullMethodWithReturnType = JavaCG2ClassMethodUtil.genFullMethodWithReturnType(methodCall.getCallerFullMethod(), methodCall.getCallerReturnType());

        // 以下对Map及Set的处理会并发执行，需要串行执行，避免添加的数据丢失
        synchronized (AbstractRunnerGenAllCallGraph.class) {
            MultiCallInfo multiCallInfo = methodCallMap.computeIfAbsent(callerFullMethodWithReturnType, k -> new MultiCallInfo(callerMethodHash, new HashSet<>()));
            Set<FullMethodWithReturnType> calleeMethodSet = multiCallInfo.getCalleeMethodSet();
            calleeMethodSet.add(new FullMethodWithReturnType(methodCall.getCalleeFullMethod(), methodCall.getRawReturnType()));
            if (calleeMethodSet.size() > 1) {
                multiCallerFullMethodSet.add(new FullMethodWithReturnType(methodCall.getCallerFullMethod(), methodCall.getCallerReturnType()));
            }
        }

        return true;
    }

    // 记录被禁用的方法调用
    protected boolean recordDisabledMethodCall(int callId, String callType) {
        JavaCG2CallTypeEnum callTypeEnum = JavaCG2CallTypeEnum.getFromType(callType);
        if (JavaCG2CallTypeEnum.CTE_INTERFACE_CALL_IMPL_CLASS != callTypeEnum && JavaCG2CallTypeEnum.CTE_SUPER_CALL_CHILD != callTypeEnum) {
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

        MultiCallInfo multiCallInfo = methodCallMap.computeIfAbsent(callerFullMethod, k -> new MultiCallInfo(callerMethodHash, ConcurrentHashMap.newKeySet()));
        multiCallInfo.getCalleeMethodSet().add(new FullMethodWithReturnType(methodCall.getCalleeFullMethod(), methodCall.getRawReturnType()));
        return true;
    }

    // 打印存在一对多的方法调用
    private void printMultiMethodCall(Map<String, MultiCallInfo> methodCallMap, Set<FullMethodWithReturnType> multiCallerFullMethodSet, JavaCG2CallTypeEnum callTypeEnum) {
        // 判断相关存在一对多的调用方方法是否有被其他方法调用，若未被调用则不显示
        List<FullMethodWithReturnType> multiCallerFullMethodList = new ArrayList<>(multiCallerFullMethodSet.size());
        for (FullMethodWithReturnType multiCallerMethod : multiCallerFullMethodSet) {
            String multiCallerMethodHash = JACGClassMethodUtil.genMethodHashWithLen(multiCallerMethod.getFullMethod(), multiCallerMethod.getReturnType());
            if (methodCallHandler.checkExistsNormalMethodCallByCalleeMethodHash(multiCallerMethodHash)) {
                // 当前存在一对多的调用方方法有被其他方法调用
                multiCallerFullMethodList.add(multiCallerMethod);
            } else {
                logger.warn("当前存在一对多的调用方方法未被其他方法调用，不打印到文件中 {} {}", multiCallerMethod.getFullMethod(), multiCallerMethod.getReturnType());
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

            multiCallerFullMethodList.sort(Comparator4FullMethodWithReturnType.getInstance());
            for (FullMethodWithReturnType multiCallerMethod : multiCallerFullMethodList) {
                MultiCallInfo multiCallInfo = methodCallMap.get(multiCallerMethod);
                if (multiCallInfo == null || JavaCG2Util.isCollectionEmpty(multiCallInfo.getCalleeMethodSet())) {
                    logger.warn("未查找到对应的一对多方法调用关系 {}", multiCallerMethod);
                    continue;
                }

                markdownWriter.addTitle(2, multiCallerMethod.genFullMethodWithReturnType());
                markdownWriter.addListWithNewLine(DC.MC_CALLER_METHOD_HASH);
                markdownWriter.addLineWithNewLine(multiCallInfo.getCallerMethodHash());
                markdownWriter.addListWithNewLine(DC.MC_CALLEE_FULL_METHOD + "（被调用的方法）");
                markdownWriter.addCodeBlock();
                List<FullMethodWithReturnType> calleeMethodList = new ArrayList<>(multiCallInfo.getCalleeMethodSet());
                calleeMethodList.sort(Comparator4FullMethodWithReturnType.getInstance());
                for (FullMethodWithReturnType calleeMethod : calleeMethodList) {
                    markdownWriter.addLine(calleeMethod.genFullMethodWithReturnType());
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
                if (multiCallInfo == null || JavaCG2Util.isCollectionEmpty(multiCallInfo.getCalleeMethodSet())) {
                    logger.error("未查找到对应的被禁用方法调用关系 {}", disabledCallerMethod);
                    continue;
                }
                markdownWriter.addTitle(2, disabledCallerMethod);
                markdownWriter.addListWithNewLine(DC.MC_CALLER_METHOD_HASH);
                markdownWriter.addLineWithNewLine(multiCallInfo.getCallerMethodHash());
                markdownWriter.addListWithNewLine(DC.MC_CALLEE_FULL_METHOD + "（被调用的方法）");

                markdownWriter.addCodeBlock();
                for (FullMethodWithReturnType calleeMethod : multiCallInfo.getCalleeMethodSet()) {
                    markdownWriter.addLine(calleeMethod.genFullMethodWithReturnType());
                }
                markdownWriter.addCodeBlock();
            }
        } catch (Exception e) {
            logger.error("error ", e);
        }
    }

    // 打印提示信息
    protected void printNoticeInfo() {
        if (!callGraphWriteToFile) {
            return;
        }
        printMultiMethodCall(itfMethodCallMap, itfMultiCallerMethodSet, JavaCG2CallTypeEnum.CTE_INTERFACE_CALL_IMPL_CLASS);
        printMultiMethodCall(sccMethodCallMap, sccMultiCallerMethodSet, JavaCG2CallTypeEnum.CTE_SUPER_CALL_CHILD);
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
        // 检查H2数据库文件是否可写，不允许文件不存在
        return checkH2DbFileWritable(false);
    }

    // 添加用于添加对方法上的注解进行处理的类
    protected boolean addMethodAnnotationHandlerExtensions() {
        List<String> methodAnnotationHandlerClassList = configureWrapper.getOtherConfigList(OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_METHOD_ANNOTATION_FORMATTER);
        if (JavaCG2Util.isCollectionEmpty(methodAnnotationHandlerClassList)) {
            annotationFormatterList = Collections.emptyList();
            return true;
        }

        annotationFormatterList = new ArrayList<>(methodAnnotationHandlerClassList.size());
        try {
            for (String extensionClass : methodAnnotationHandlerClassList) {
                AbstractAnnotationFormatter annotationFormatter = JACGClassMethodUtil.genClassObject(extensionClass, AbstractAnnotationFormatter.class);
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
            if (DefaultBusinessDataTypeEnum.BDTE_ILLEGAL != businessDataTypeEnum) {
                // 找到默认的业务功能数据类型
                if (order4ee && !businessDataTypeEnum.isSupportEe()) {
                    logger.error("当前指定的业务功能数据类型 {} 不支持在生成向上的方法完整调用链时显示，请删除 {}", businessDataTypeEnum,
                            configureWrapper.genConfigUsage(OtherConfigFileUseSetEnum.OCFULE_BUSINESS_DATA_TYPE_SHOW_4EE));
                    return false;
                }
                if (!order4ee && !businessDataTypeEnum.isSupportEr()) {
                    logger.error("当前指定的业务功能数据类型 {} 不支持在生成向下的方法完整调用链时显示，请删除 {}", businessDataTypeEnum,
                            configureWrapper.genConfigUsage(OtherConfigFileUseSetEnum.OCFULE_BUSINESS_DATA_TYPE_SHOW_4ER));
                    return false;
                }
            }
        }

        // todo
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
     * @param methodCallLineStr  方法调用当前行字符串
     * @param methodCallLineData 方法调用当前行数据
     * @return 当前方法上的注解信息
     */
    protected void getMethodAnnotationInfo(StringBuilder methodCallLineStr, MethodCallLineData methodCallLineData) {
        Integer callFlags = methodCallLineData.getCallFlags();
        if (callFlags != null) {
            // 若方法调用标志非空，则需要判断对应方法是否存在注解
            if ((order4ee && !MethodCallFlagsEnum.MCFE_ER_METHOD_ANNOTATION.checkFlag(callFlags)) ||
                    (!order4ee && !MethodCallFlagsEnum.MCFE_EE_METHOD_ANNOTATION.checkFlag(callFlags))) {
                 /*
                    生成向上的方法完整调用链时，判断调用方法是否存在注解
                    生成向下的方法完整调用链时，判断被调用方法是否存在注解
                 */
                return;
            }
        }

        // 根据完整方法HASH+长度获取对应的注解信息
        Map<String, Map<String, BaseAnnotationAttribute>> methodAnnotationMap =
                annotationHandler.queryAnnotationMap4MethodWithReturnType(methodCallLineData.getActualFullMethod(), methodCallLineData.getMethodReturnType());
        if (methodAnnotationMap == null) {
            // 当前方法上没有注解
            return;
        }

        // 当前方法上有注解
        methodCallLineData.setMethodAnnotationMap(methodAnnotationMap);
        String existedAnnotationStr = methodAllAnnotationStrMap.get(methodCallLineData.getActualMethodHash());
        if (existedAnnotationStr != null) {
            // 当前方法对应的注解信息已查询过，直接使用
            methodCallLineStr.append(existedAnnotationStr);
            return;
        }

        // 当前方法对应的注解信息未查询过
        StringBuilder stringBuilder = new StringBuilder();

        // 遍历当前方法上的所有注解进行处理
        List<String> annotationNameList = new ArrayList<>(methodAnnotationMap.keySet());
        Collections.sort(annotationNameList);
        for (String annotationName : annotationNameList) {
            Map<String, BaseAnnotationAttribute> annotationAttributeMap = methodAnnotationMap.get(annotationName);
            // 遍历用于对方法上的注解进行处理的类
            for (AbstractAnnotationFormatter annotationFormatter : annotationFormatterList) {
                if (!annotationFormatter.checkHandleAnnotation(annotationName)) {
                    continue;
                }

                // 找到能够处理的类进行处理
                String className = JavaCG2ClassMethodUtil.getClassNameFromMethod(methodCallLineData.getActualFullMethod());
                String annotationInfo = annotationFormatter.handleAnnotation(methodCallLineData.getActualFullMethod(), methodCallLineData.getMethodReturnType(), className,
                        annotationName, annotationAttributeMap);
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
        // 记录当前方法已查询过的注解信息
        methodAllAnnotationStrMap.putIfAbsent(methodCallLineData.getActualMethodHash(), allAnnotationInfo);
        methodCallLineStr.append(allAnnotationInfo);
    }

    /**
     * 通过代码行号获取对应方法
     *
     * @param isCallee        true: 生成向上的方法完整调用链 false: 生成向下的方法完整调用链
     * @param simpleClassName
     * @param methodLineNum
     * @return
     */
    protected FindMethodTaskInfo findMethodByLineNumber(boolean isCallee, String simpleClassName, int methodLineNum) {
        WriteDbData4MethodLineNumber writeDbData4MethodLineNumber = dbOperWrapper.queryMethodLineNumber(simpleClassName, methodLineNum);
        if (writeDbData4MethodLineNumber == null) {
            logger.warn("指定类 {} 的代码行号 {} 未查找到对应方法，请检查，可能因为以下原因 " +
                    "1. 指定的类所在的jar包未在配置文件中指定 {} " +
                    "2. 指定的方法是接口中未实现的方法 " +
                    "3. 指定的方法是抽象方法", simpleClassName, methodLineNum, configureWrapper.genConfigUsage(JavaCG2OtherConfigFileUseListEnum.OCFULE_JAR_DIR));
            return FindMethodTaskInfo.genFindMethodInfoGenNotFoundFile();
        }

        // 查询方法的标志
        WriteDbData4MethodCall methodCallExtraInfo = queryMethodCallExtraInfo(isCallee, writeDbData4MethodLineNumber.getMethodHash());
        String methodReturnType = isCallee ? methodCallExtraInfo.getRawReturnType() : methodCallExtraInfo.getCallerReturnType();
        // 指定类的代码行号查找到对应方法
        FindMethodTaskInfo findMethodTaskInfo = FindMethodTaskInfo.genFindMethodInfoSuccess();
        findMethodTaskInfo.addTaskElement(writeDbData4MethodLineNumber.getMethodHash(), writeDbData4MethodLineNumber.getFullMethod(), methodReturnType);
        return findMethodTaskInfo;
    }

    /**
     * 查询方法调用额外信息，包括调用方法返回类型、方法调用标志、被调用方法原始返回类型等
     *
     * @param isCallee   true: 生成向上的方法完整调用链 false: 生成向下的方法完整调用链
     * @param methodHash
     * @return
     */
    private WriteDbData4MethodCall queryMethodCallExtraInfo(boolean isCallee, String methodHash) {
        // 查询方法调用的额外信息
        WriteDbData4MethodCall writeDbData4MethodCall = dbOperWrapper.queryMethodCallExtraInfo(isCallee, methodHash);
        if (writeDbData4MethodCall == null) {
            writeDbData4MethodCall = new WriteDbData4MethodCall();
            writeDbData4MethodCall.setCallerReturnType("");
            writeDbData4MethodCall.setRawReturnType("");

            // 未查询到方法调用时需要继续查方法信息
            WriteDbData4MethodInfo methodInfo = methodInfoHandler.queryMethodInfoByMethodHash(methodHash);
            if (methodInfo != null) {
                if (isCallee) {
                    writeDbData4MethodCall.setRawReturnType(methodInfo.getReturnType());
                } else {
                    writeDbData4MethodCall.setCallerReturnType(methodInfo.getReturnType());
                }
            }
        }
        return writeDbData4MethodCall;
    }

    /**
     * 判断当前找到的方法调用是否需要忽略
     *
     * @param callType
     * @param callerFullMethod
     * @param calleeFullMethod
     * @param methodCallFlags
     * @return false: 不忽略 true: 忽略
     */
    protected boolean ignoreCurrentMethodCall(String callType, String callerFullMethod, String calleeFullMethod, int methodCallFlags) {
        if (callerFullMethod == null || calleeFullMethod == null) {
            // 若调用方法或被调用方法为null，说明为起始方法，不忽略
            return false;
        }
        return elManager.checkIgnoreMethodCall(callType, callerFullMethod, calleeFullMethod, methodCallFlags);
    }

    /**
     * 为方法调用信息增加是否在其他线程执行标志
     *
     * @param callInfo
     * @param methodCallId
     * @param callType
     */
    protected void addRunInOtherThread(StringBuilder callInfo, int methodCallId, String callType, MethodCallLineData methodCallLineData) {
        if (JavaCG2CallTypeEnum.isRunInOtherThreadType(callType)) {
            // 方法调用类型属于线程调用，在方法调用上增加在其他线程执行的标志
            doAddRunInOtherThread(callInfo, methodCallLineData);
            return;
        }

        Map<String, Map<String, BaseAnnotationAttribute>> methodAnnotationMap = methodCallLineData.getMethodAnnotationMap();
        if (methodAnnotationMap != null && methodAnnotationMap.get(JACGCommonNameConstants.SPRING_ASYNC_ANNOTATION) != null) {
            // 方法上的注解包括@Async，在方法调用上增加在其他线程执行的标志
            doAddRunInOtherThread(callInfo, methodCallLineData);
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
                doAddRunInOtherThread(callInfo, methodCallLineData);
            }
        }
    }

    // 在方法调用上增加在其他线程执行的标志
    private void doAddRunInOtherThread(StringBuilder callInfo, MethodCallLineData methodCallLineData) {
        callInfo.append(JACGConstants.CALL_FLAG_RUN_IN_OTHER_THREAD);
        methodCallLineData.setRunInOtherThread(true);
    }

    // 为方法调用信息增加是否在Spring事务中执行标志
    protected void addRunInSpringTransaction(StringBuilder callInfo, int methodCallId, String callType, MethodCallLineData methodCallLineData) {
        if (JavaCG2CallTypeEnum.isRunInSpringTxType(callType)) {
            // 方法调用类型属于事务调用，在方法调用上增加在事务中执行的标志
            doAddRunInSpringTransaction(callInfo, methodCallLineData);
            return;
        }

        Map<String, Map<String, BaseAnnotationAttribute>> methodAnnotationMap = methodCallLineData.getMethodAnnotationMap();
        if (methodAnnotationMap != null && methodAnnotationMap.get(JACGCommonNameConstants.SPRING_TX_ANNOTATION) != null) {
            // 方法上的注解包括@Transactional，在方法调用上增加在事务中执行的标志
            doAddRunInSpringTransaction(callInfo, methodCallLineData);
            return;
        }

        if (JavaCG2CallTypeEnum.CTE_LAMBDA.getType().equals(callType)) {
            WriteDbData4LambdaMethodInfo lambdaCalleeInfo = lambdaMethodHandler.queryLambdaCalleeInfo(methodCallId);
            if (lambdaCalleeInfo != null && (
                    (JavaCG2CommonNameConstants.CLASS_NAME_TRANSACTION_CALLBACK.equals(lambdaCalleeInfo.getLambdaCalleeClassName()) &&
                            JavaCG2CommonNameConstants.METHOD_NAME_DO_IN_TRANSACTION.equals(lambdaCalleeInfo.getLambdaCalleeMethodName()))
            )) {
                // 方法为Lambda表达式，且属于事务调用，在方法调用上增加在事务中执行的标志
                doAddRunInSpringTransaction(callInfo, methodCallLineData);
            }
        }
    }

    // 在方法调用上增加在Spring事务中执行的标志
    private void doAddRunInSpringTransaction(StringBuilder callInfo, MethodCallLineData methodCallLineData) {
        callInfo.append(JACGConstants.CALL_FLAG_RUN_IN_SPRING_TX);
        methodCallLineData.setRunInTransaction(true);
    }

    /**
     * 添加方法调用业务功能数据
     *
     * @param methodCallId       方法调用序号
     * @param callFlags          方法调用标志
     * @param methodHash         对应的方法HASH+长度
     * @param callGraphInfo      调用信息
     * @param methodCallLineData 方法完整调用链当前行的数据
     * @return
     */
    protected boolean addBusinessData(int methodCallId,
                                      Integer callFlags,
                                      String methodHash,
                                      StringBuilder callGraphInfo,
                                      MethodCallLineData methodCallLineData) {
        // 添加默认的方法调用业务功能数据
        if (!addDefaultBusinessData(methodCallId, callFlags, methodHash, callGraphInfo, methodCallLineData)) {
            return false;
        }

        if (callFlags != null && !MethodCallFlagsEnum.MCFE_EE_BUSINESS_DATA.checkFlag(callFlags)) {
            // 被调用方法不存在业务功能数据时，不需要处理
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

        List<BaseBusinessData> businessDataList = dbOperator.queryList(sql, BaseBusinessData.class, methodCallId);
        if (JavaCG2Util.isCollectionEmpty(businessDataList)) {
            if (callFlags == null) {
                return true;
            }
            logger.error("查询方法调用业务功能数据不存在 {}", methodCallId);
            return false;
        }

        for (BaseBusinessData businessData : businessDataList) {
            // 将方法调用业务功能数据加入被调用方法信息中
            addBusinessData2CallGraphInfo(businessData.getDataType(), businessData.getDataValue(), callGraphInfo, methodCallLineData);
        }
        return true;
    }

    // 添加默认的方法调用业务功能数据
    private boolean addDefaultBusinessData(int methodCallId,
                                           Integer callFlags,
                                           String methodHash,
                                           StringBuilder callGraphInfo,
                                           MethodCallLineData methodCallLineData) {
        for (String businessDataType : businessDataTypeList) {
            DefaultBusinessDataTypeEnum defaultBusinessDataTypeEnum = DefaultBusinessDataTypeEnum.getFromType(businessDataType);
            switch (defaultBusinessDataTypeEnum) {
                case BDTE_METHOD_CALL_INFO:
                    // 显示方法调用信息
                    if (callFlags != null && !MethodCallFlagsEnum.MCFE_METHOD_CALL_INFO.checkFlag(callFlags)) {
                        continue;
                    }

                    ObjArgsInfoInMethodCall objArgsInfoInMethodCall = methodCallInfoHandler.queryObjArgsInfoInMethodCall(methodCallId);
                    if (objArgsInfoInMethodCall != null) {
                        addBusinessData2CallGraphInfo(businessDataType, objArgsInfoInMethodCall, callGraphInfo, methodCallLineData);
                    } else if (callFlags != null) {
                        logger.error("未查询到方法调用信息 {}", methodCallId);
                        return false;
                    }
                    break;
                case BDTE_METHOD_ARG_GENERICS_TYPE:
                    // 显示方法参数泛型类型
                    if (!addMethodArgsGenericsTypeInfo(callFlags, methodHash, callGraphInfo, methodCallLineData)) {
                        return false;
                    }
                    break;
                case BDTE_METHOD_RETURN_GENERICS_TYPE:
                    // 显示方法返回泛型类型
                    if (!addMethodReturnGenericsTypeInfo(callFlags, methodHash, callGraphInfo, methodCallLineData)) {
                        return false;
                    }
                    break;
                default:
                    break;
            }
        }
        return true;
    }

    /**
     * 显示方法参数泛型类型
     *
     * @param callFlags          方法调用标志
     * @param methodHash         完整方法HASH+长度
     * @param callGraphInfo      调用链信息
     * @param methodCallLineData 方法完整调用链当前行的数据
     * @return
     */
    protected boolean addMethodArgsGenericsTypeInfo(Integer callFlags, String methodHash, StringBuilder callGraphInfo, MethodCallLineData methodCallLineData) {
        if (callFlags != null) {
            // 方法调用标志非空
            if ((order4ee && !MethodCallFlagsEnum.MCFE_ER_ARGS_WITH_GENERICS_TYPE.checkFlag(callFlags)) ||
                    (!order4ee && !MethodCallFlagsEnum.MCFE_EE_ARGS_WITH_GENERICS_TYPE.checkFlag(callFlags))) {
                 /*
                    生成向上的方法完整调用链时，判断调用方法是否存在参数泛型类型
                    生成向下的方法完整调用链时，判断被调用方法是否存在参数泛型类型
                 */
                return true;
            }
        }

        MethodArgGenericsTypeInfo methodArgGenericsTypeInfo = methodArgReturnHandler.queryArgsGenericsTypeInfo(methodHash);
        if (methodArgGenericsTypeInfo == null) {
            if (callFlags == null) {
                return true;
            }
            logger.error("未查询到方法参数的泛型信息 {}", methodHash);
            return false;
        }
        methodCallLineData.setMethodArgGenericsTypeInfo(methodArgGenericsTypeInfo);
        addBusinessData2CallGraphInfo(DefaultBusinessDataTypeEnum.BDTE_METHOD_ARG_GENERICS_TYPE.getType(), methodArgGenericsTypeInfo, callGraphInfo, methodCallLineData);
        return true;
    }

    /**
     * 显示方法返回泛型类型
     *
     * @param callFlags
     * @param methodHash
     * @param callGraphInfo
     * @param methodCallLineData
     * @return
     */
    protected boolean addMethodReturnGenericsTypeInfo(Integer callFlags, String methodHash, StringBuilder callGraphInfo,
                                                      MethodCallLineData methodCallLineData) {
        if (callFlags != null) {
            // 方法调用标志非空
            if ((order4ee && !MethodCallFlagsEnum.MCFE_ER_RETURN_WITH_GENERICS_TYPE.checkFlag(callFlags)) ||
                    (!order4ee && !MethodCallFlagsEnum.MCFE_EE_RETURN_WITH_GENERICS_TYPE.checkFlag(callFlags))) {
                 /*
                    生成向上的方法完整调用链时，判断调用方法是否存在返回泛型类型
                    生成向下的方法完整调用链时，判断被调用方法是否存在返回泛型类型
                 */
                return true;
            }
        }

        GenericsTypeValue methodReturnGenericsTypeInfo = methodArgReturnHandler.queryReturnGenericsTypeInfoByMethodHash(methodHash);
        if (methodReturnGenericsTypeInfo == null) {
            if (callFlags == null) {
                return true;
            }
            logger.error("未查询到方法返回类型中的泛型信息 {}", methodHash);
            return false;
        }
        methodCallLineData.setMethodReturnGenericsTypeInfo(methodReturnGenericsTypeInfo);
        addBusinessData2CallGraphInfo(DefaultBusinessDataTypeEnum.BDTE_METHOD_RETURN_GENERICS_TYPE.getType(), methodReturnGenericsTypeInfo, callGraphInfo, methodCallLineData);
        return true;
    }

    /**
     * 将方法调用业务功能数据加入被调用方法信息中
     *
     * @param dataType           方法调用业务功能数据类型
     * @param dataValue          方法调用业务功能数据值
     * @param callGraphInfo      方法调用信息
     * @param methodCallLineData 方法完整调用链当前行信息
     */
    protected void addBusinessData2CallGraphInfo(String dataType, Object dataValue, StringBuilder callGraphInfo, MethodCallLineData methodCallLineData) {
        String jsonStr = JACGJsonUtil.getJsonStr(dataValue);
        addBusinessData2CallGraphInfo(dataType, jsonStr, callGraphInfo, methodCallLineData);
    }

    /**
     * 将方法调用业务功能数据加入被调用方法信息中
     *
     * @param dataType           方法调用业务功能数据类型
     * @param dataValue          方法调用业务功能数据值
     * @param callGraphInfo      方法调用信息
     * @param methodCallLineData 方法完整调用链当前行信息
     */
    protected void addBusinessData2CallGraphInfo(String dataType, String dataValue, StringBuilder callGraphInfo, MethodCallLineData methodCallLineData) {
        callGraphInfo.append(JavaCG2Constants.FLAG_TAB)
                .append(JACGConstants.CALL_FLAG_BUSINESS_DATA)
                .append(dataType)
                .append(JACGConstants.FLAG_AT)
                .append(dataValue);

        methodCallLineData.setBusinessDataType(dataType);
        methodCallLineData.setBusinessDataValue(dataValue);
    }

    /**
     * 处理方法调用数量已达到限制的方法
     *
     * @param inputMethod
     * @param callGraphNum
     * @param writer
     * @param methodCallLineDataList
     * @throws IOException
     */
    protected void handleNumExceedMethod(String inputMethod, int callGraphNum, BufferedWriter writer, List<? extends MethodCallLineData> methodCallLineDataList) throws IOException {
        logger.warn("当前方法调用数量已达到限制，不再生成 {} {}", inputMethod, callGraphNum);
        genCallGraphNumExceedMethodList.add(inputMethod);
        if (JavaCG2Util.isCollectionEmpty(methodCallLineDataList)) {
            // 列表为空时，不需要写入文件
            return;
        }

        StringBuilder callGraphInfo = callGraphWriteToFile ? new StringBuilder() : null;
        for (MethodCallLineData methodCallLineData : methodCallLineDataList) {
            // 生成方法完整调用链每行数据字符串
            String lineData = genMethodCallLineStr(methodCallLineData);
            if (callGraphInfo != null) {
                callGraphInfo.append(lineData).append(JavaCG2Constants.NEW_LINE);
            }
        }
        if (callGraphInfo != null) {
            // 将剩余的调用方法信息写入文件
            writer.write(callGraphInfo.toString());
        }
    }

    // 结束前的处理
    @Override
    protected void beforeExit() {
        super.beforeExit();

        if (!genCallGraphNumExceedMethodList.isEmpty()) {
            logger.warn("生成方法完整调用链时，生成的方法调用数量超过限制的方法 {} {}", genCallGraphNumLimit, StringUtils.join(genCallGraphNumExceedMethodList, " "));
        }

        // 关闭表达式管理类
        if (elManager != null) {
            elManager.close();
        }
    }

    /**
     * 将调用链文件重命名为代表空的调用链
     *
     * @param callGraphFilePath
     */
    protected void renameCallGraphFile2Empty(String callGraphFilePath) {
        String filePathHead = StringUtils.substringBeforeLast(callGraphFilePath, JavaCG2Constants.EXT_TXT);
        String emptyCallGraphFilePath = filePathHead + JACGConstants.EMPTY_TXT;
        if (!JACGFileUtil.renameFile(callGraphFilePath, emptyCallGraphFilePath)) {
            throw new JavaCG2RuntimeException("将调用链文件重命名为代表空的调用链失败");
        }
    }

    /**
     * 选择生成的调用链文件名
     *
     * @param startCallSimpleClassName
     * @param startMethodName
     * @param startCallMethodHash
     * @return
     */
    protected String chooseCallGraphFileName(String startCallSimpleClassName, String startMethodName, String startCallMethodHash) {
        if (callGraphFileShortMode) {
            return startCallMethodHash;
        }
        return JACGCallGraphFileUtil.genCallGraphMethodFileName(startCallSimpleClassName, startMethodName, startCallMethodHash);
    }

    // 获取生成向上/向下的方法完整调用链时，生成的方法调用数量超过限制的方法
    public List<String> getGenCallGraphNumExceedMethodListReadOnly() {
        return Collections.unmodifiableList(genCallGraphNumExceedMethodList);
    }
}
