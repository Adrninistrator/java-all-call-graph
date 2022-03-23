package com.adrninistrator.jacg.runner;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.conf.ConfInfo;
import com.adrninistrator.jacg.conf.ConfManager;
import com.adrninistrator.jacg.dto.CallerTaskInfo;
import com.adrninistrator.jacg.dto.MultiImplMethodInfo;
import com.adrninistrator.jacg.dto.TmpNode4Caller;
import com.adrninistrator.jacg.extensions.dto.BaseExtendedData;
import com.adrninistrator.jacg.extensions.enums.ExtendedDataResultEnum;
import com.adrninistrator.jacg.extensions.extended_data_add.ExtendedDataAddInterface;
import com.adrninistrator.jacg.extensions.extended_data_supplement.ExtendedDataSupplementInterface;
import com.adrninistrator.jacg.runner.base.AbstractRunnerGenCallGraph;
import com.adrninistrator.jacg.util.FileUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg.enums.CallTypeEnum;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

/**
 * @author adrninistrator
 * @date 2021/6/18
 * @description: 从数据库读取数据，生成指定类调用的所有向下的调用关系
 */

public class RunnerGenAllGraph4Caller extends AbstractRunnerGenCallGraph {

    private static final Logger logger = LoggerFactory.getLogger(RunnerGenAllGraph4Caller.class);

    // 需要忽略的入口方法前缀
    protected Set<String> entryMethodIgnorePrefixSet;

    // 完整方法（类名+方法名+参数）为以下前缀时，忽略
    private Set<String> ignoreFullMethodPrefixSet;

    // 当类名包含以下关键字时，忽略
    private Set<String> ignoreClassKeywordSet;

    // 当方法名为以下前缀时，忽略
    private Set<String> ignoreMethodPrefixSet;

    // 是否支持忽略指定方法
    private boolean supportIgnore = false;

    // 保存存在自定义数据的方法调用序号
    private Set<Integer> callIdWithExtendedDataSet;

    // 保存存在手工添加的自定义数据的调用方与被调用方完整方法
    private Map<String, Set<String>> callFullMethodWithMAEDMap;

    // 保存用于添加自定义数据的处理类
    private List<ExtendedDataAddInterface> extendedDataAddExtList;

    // 保存用于对自定义数据进行补充的处理类
    private Map<String, ExtendedDataSupplementInterface> extendedDataSupplementExtMap;

    // 存在多个实现类的接口方法HASH
    private Set<String> multiImplMethodHashSet;

    // 存在多个子类的父类方法HASH
    private Set<String> multiChildrenMethodHashSet;

    // 本次执行时查询到存在多个实现类的接口或父类方法信息
    private Map<String, MultiImplMethodInfo> currentFoundMultiImplMethodMap;

    // 所有查询到存在多个实现类的接口或父类方法信息
    private Map<String, Boolean> allFoundMultiImplMethodMap = new ConcurrentHashMap<>();

    // 用于打印的向自定义数据表插入的SQL语句
    private static String insertManualTableSql = null;

    static {
        runner = new RunnerGenAllGraph4Caller();
    }

    // 返回用于打印的向自定义数据表插入的SQL语句
    public static String getInsertManualTableSql() {
        if (insertManualTableSql != null) {
            return insertManualTableSql;
        }

        ConfInfo confInfo = ConfManager.getConfInfo();
        if (confInfo == null) {
            return null;
        }
        insertManualTableSql = "insert into " + JACGConstants.TABLE_PREFIX_MANUAL_ADD_EXTENDED_DATA + confInfo.getAppName() +
                " (" + StringUtils.join(JACGConstants.TABLE_COLUMNS_MANUAL_ADD_EXTENDED_DATA, JACGConstants.FLAG_COMMA_WITH_SPACE) +
                ") values (\n" +
                "'',\n" +
                "'',\n" +
                ",\n" +
                "'',\n" +
                "'');";
        return insertManualTableSql;
    }

    @Override
    public boolean init() {
        // 检查Jar包文件是否有更新
        if (checkJarFileUpdated()) {
            return false;
        }

        String taskInfoFile = JACGConstants.DIR_CONFIG + File.separator + JACGConstants.FILE_OUT_GRAPH_FOR_CALLER_ENTRY_METHOD;
        // 读取配置文件中指定的需要处理的任务
        if (!readTaskInfo(taskInfoFile)) {
            return false;
        }

        String entryMethodIgnorePrefixFile = JACGConstants.DIR_CONFIG + File.separator + JACGConstants.FILE_OUT_GRAPH_FOR_CALLER_ENTRY_METHOD_IGNORE_PREFIX;
        entryMethodIgnorePrefixSet = FileUtil.readFile2Set(entryMethodIgnorePrefixFile);
        if (entryMethodIgnorePrefixSet == null) {
            return false;
        }

        // 创建输出文件所在目录
        if (!createOutputDir(JACGConstants.DIR_OUTPUT_GRAPH_FOR_CALLER)) {
            return false;
        }

        ignoreClassKeywordSet =
                FileUtil.readFile2Set(JACGConstants.DIR_CONFIG + File.separator + JACGConstants.FILE_OUT_GRAPH_FOR_CALLER_IGNORE_CLASS_KEYWORD);
        if (ignoreClassKeywordSet == null) {
            return false;
        }

        ignoreFullMethodPrefixSet =
                FileUtil.readFile2Set(JACGConstants.DIR_CONFIG + File.separator + JACGConstants.FILE_OUT_GRAPH_FOR_CALLER_IGNORE_FULL_METHOD_PREFIX);
        if (ignoreFullMethodPrefixSet == null) {
            return false;
        }

        ignoreMethodPrefixSet =
                FileUtil.readFile2Set(JACGConstants.DIR_CONFIG + File.separator + JACGConstants.FILE_OUT_GRAPH_FOR_CALLER_IGNORE_METHOD_PREFIX);
        if (ignoreMethodPrefixSet == null) {
            return false;
        }

        // 添加用于添加自定义数据处理类
        if (!addExtendedDataAddExtensions()) {
            return false;
        }

        // 添加用于对自定义数据进行补充的处理类
        if (!addExtendedDataSupplementExtensions()) {
            return false;
        }

        // 查询存在多个实现类的接口或父类方法HASH
        if (!queryMultiImplMethodHash()) {
            return false;
        }

        return true;
    }

    @Override
    public void operate() {
        if (!doOperate()) {
            someTaskFail = true;
            return;
        }

        if (someTaskFail) {
            return;
        }

        // 将输出文件合并
        combineOutputFile(JACGConstants.COMBINE_FILE_NAME_4_CALLER);
    }

    private boolean doOperate() {
        logger.info("{}忽略指定的方法", isSupportIgnore() ? "" : "不");

        // 读取方法注解
        if (confInfo.isShowMethodAnnotation() && !readMethodAnnotation()) {
            return false;
        }

        // 查询存在自定义数据的方法调用序号
        if (!queryCallIdWithExtendedData()) {
            return false;
        }

        // 查询存在手工添加的自定义数据的调用方与被调用方完整方法
        if (!queryCallFullMethodWithMAEDMap()) {
            return false;
        }

        // 创建线程
        createThreadPoolExecutor();

        // 生成文件中指定的需要执行的任务信息
        List<CallerTaskInfo> callerTaskInfoList = genCallerTaskInfo();
        if (JACGUtil.isCollectionEmpty(callerTaskInfoList)) {
            return false;
        }

        // 执行任务并等待
        runAndWait(callerTaskInfoList);

        while (true) {
            // 根据记录本次执行时查询到存在多个实现类的接口或父类方法信息，生成继续执行的任务信息
            List<CallerTaskInfo> callerTaskInfoList2 = genTaskFromMultiImplMethod();
            if (JACGUtil.isCollectionEmpty(callerTaskInfoList2)) {
                logger.info("需要继续执行的任务已执行完毕");
                break;
            }

            // 重新设置线程数，若当前线程数需要调大则修改
            resetPoolSize(callerTaskInfoList2.size());

            // 执行任务并等待
            runAndWait(callerTaskInfoList2);
        }

        return true;
    }

    // 设置生成调用链时的详细程度为最详细
    public static void setCallGraphOutputDetailMost() {
        System.setProperty(JACGConstants.KEY_CALL_GRAPH_OUTPUT_DETAIL, JACGConstants.CONFIG_OUTPUT_DETAIL_1);

        if (ConfManager.isInited()) {
            // 当已经完成初始化配置时，再修改对应设置
            ConfInfo confInfo = ConfManager.getConfInfo();
            if (confInfo != null) {
                confInfo.setCallGraphOutputDetail(JACGConstants.CONFIG_OUTPUT_DETAIL_1);
            }
        }
    }

    // 设置生成向下的完整方法调用链时，忽略在一个调用方法中出现多次的被调用方法（包含自定义数据）
    public static void setIgnoreDupCalleeInOneCaller() {
        System.setProperty(JACGConstants.KEY_IGNORE_DUP_CALLEE_IN_ONE_CALLER, String.valueOf(true));

        if (ConfManager.isInited()) {
            // 当已经完成初始化配置时，再修改对应设置
            ConfInfo confInfo = ConfManager.getConfInfo();
            if (confInfo != null) {
                confInfo.setIgnoreDupCalleeInOneCaller(true);
            }
        }
    }

    // 设置生成向下的完整方法调用链时，显示调用者源代码行号
    public static void setShowCallerLineNum() {
        System.setProperty(JACGConstants.KEY_SHOW_CALLER_LINE_NUM, String.valueOf(true));

        if (ConfManager.isInited()) {
            // 当已经完成初始化配置时，再修改对应设置
            ConfInfo confInfo = ConfManager.getConfInfo();
            if (confInfo != null) {
                confInfo.setShowCallerLineNum(true);
            }
        }
    }

    // 执行任务并等待
    private void runAndWait(List<CallerTaskInfo> callerTaskInfoList) {
        // 本次执行时查询到存在多个实现类的接口或父类方法信息，初始化
        currentFoundMultiImplMethodMap = new ConcurrentHashMap<>();

        // 遍历需要处理的任务
        for (CallerTaskInfo callerTaskInfo : callerTaskInfoList) {
            // 等待直到允许任务执行
            wait4TPEExecute();

            threadPoolExecutor.execute(() -> {
                // 处理一条记录
                if (!handleOneRecord(callerTaskInfo)) {
                    someTaskFail = true;
                }
            });
        }

        // 等待直到任务执行完毕
        wait4TPEDone();
    }

    // 根据记录本次执行时查询到存在多个实现类的接口或父类方法信息，生成继续执行的任务信息
    private List<CallerTaskInfo> genTaskFromMultiImplMethod() {
        if (JACGUtil.isMapEmpty(currentFoundMultiImplMethodMap)) {
            return null;
        }

        List<CallerTaskInfo> callerTaskInfoList = new ArrayList<>();

        for (Map.Entry<String, MultiImplMethodInfo> currentFoundMultiImplMethod : currentFoundMultiImplMethodMap.entrySet()) {
            String sqlKey = JACGConstants.SQL_KEY_MC_QUERY_IMPL_METHODS;
            String sql = sqlCacheMap.get(sqlKey);
            if (sql == null) {
                sql = "select " + StringUtils.joinWith(JACGConstants.FLAG_COMMA_WITH_SPACE, DC.MC_CALLEE_CLASS_NAME, DC.MC_CALLEE_FULL_METHOD) +
                        " from " + JACGConstants.TABLE_PREFIX_METHOD_CALL + confInfo.getAppName() +
                        " where " + DC.MC_CALLER_METHOD_HASH + " = ? and " + DC.MC_CALL_TYPE + " = ? and " + DC.MC_ENABLED + " = ?";
                cacheSql(sqlKey, sql);
            }

            String methodHash = currentFoundMultiImplMethod.getKey();
            MultiImplMethodInfo multiImplMethodInfo = currentFoundMultiImplMethod.getValue();
            CallTypeEnum callType = multiImplMethodInfo.getMultiImplMethodCallType();
            String dirPath = multiImplMethodInfo.getDirPath();

            List<Map<String, Object>> list = dbOperator.queryList(sql, new Object[]{methodHash, callType.getType(), JACGConstants.ENABLED});
            if (JACGUtil.isCollectionEmpty(list)) {
                logger.error("未查找到接口或父类的实现类方法信息 {} {} {}", methodHash, callType.getType(), dirPath);
                return null;
            }

            logger.info("查找到接口或父类的实现类方法数量 {} {} {} {}", methodHash, callType.getType(), dirPath, list.size());

            for (Map<String, Object> map : list) {
                CallerTaskInfo callerTaskInfo = new CallerTaskInfo();
                callerTaskInfo.setCallerClassName((String) map.get(DC.MC_CALLEE_CLASS_NAME));

                String methodWithArgs = JACGUtil.getMethodWithArgs((String) map.get(DC.MC_CALLEE_FULL_METHOD));
                callerTaskInfo.setCallerMethodName(methodWithArgs);
                callerTaskInfo.setLineNumStart(JACGConstants.LINE_NUM_NONE);
                callerTaskInfo.setLineNumEnd(JACGConstants.LINE_NUM_NONE);
                callerTaskInfo.setSaveDirPath(dirPath);

                callerTaskInfoList.add(callerTaskInfo);
            }
        }

        logger.info("需要继续执行的任务 {}", callerTaskInfoList);
        return callerTaskInfoList;
    }

    // 添加用于添加自定义数据处理类
    private boolean addExtendedDataAddExtensions() {
        String extendedDataAddFilePath = JACGConstants.DIR_EXTENSIONS + File.separator + JACGConstants.FILE_EXTENSIONS_EXTENDED_DATA_ADD;

        Set<String> extendedDataAddClasses = FileUtil.readFile2Set(extendedDataAddFilePath);
        if (JACGUtil.isCollectionEmpty(extendedDataAddClasses)) {
            logger.info("未指定用于添加自定义数据处理类，跳过 {}", extendedDataAddFilePath);
            return true;
        }

        extendedDataAddExtList = new ArrayList<>(extendedDataAddClasses.size());

        try {
            for (String extensionClass : extendedDataAddClasses) {
                ExtendedDataAddInterface extendedDataAddExt = JACGUtil.getClassObject(extensionClass, ExtendedDataAddInterface.class);
                if (extendedDataAddExt == null) {
                    return false;
                }

                extendedDataAddExt.init();

                extendedDataAddExtList.add(extendedDataAddExt);
            }

            return true;
        } catch (Exception e) {
            logger.error("error ", e);
            return false;
        }
    }

    // 添加用于对自定义数据进行补充的处理类
    private boolean addExtendedDataSupplementExtensions() {
        String extendedDataSupplementFilePath = JACGConstants.DIR_EXTENSIONS + File.separator + JACGConstants.FILE_EXTENSIONS_EXTENDED_DATA_SUPPLEMENT;

        Set<String> extendedDataSupplementClasses = FileUtil.readFile2Set(extendedDataSupplementFilePath);
        if (JACGUtil.isCollectionEmpty(extendedDataSupplementClasses)) {
            logger.info("未指定用于对自定义数据进行补充的类，跳过 {}", extendedDataSupplementFilePath);
            return true;
        }

        extendedDataSupplementExtMap = new HashMap<>(extendedDataSupplementClasses.size());

        try {
            for (String extensionClass : extendedDataSupplementClasses) {
                ExtendedDataSupplementInterface extendedDataSupplementExt = JACGUtil.getClassObject(extensionClass, ExtendedDataSupplementInterface.class);
                if (extendedDataSupplementExt == null) {
                    return false;
                }

                extendedDataSupplementExt.init();

                ExtendedDataSupplementInterface existedClass = extendedDataSupplementExtMap.putIfAbsent(extendedDataSupplementExt.getDataType(), extendedDataSupplementExt);
                if (existedClass != null) {
                    logger.error("指定的用于对自定义数据进行补充的类，存在重复的类型 {} {} {}", extendedDataSupplementExt.getDataType(), extensionClass, existedClass.getClass().getName());
                    return false;
                }
            }

            return true;
        } catch (Exception e) {
            logger.error("error ", e);
            return false;
        }
    }

    // 查询存在多个实现类的接口或父类方法HASH
    private boolean queryMultiImplMethodHash() {
        // 只查询一次，不需要缓存
        String sql = "select " + DC.MC_CALLER_METHOD_HASH + " from " + JACGConstants.TABLE_PREFIX_METHOD_CALL + confInfo.getAppName() + " where " + DC.MC_CALL_TYPE
                + " = ? and " + DC.MC_ENABLED + " = ? group by " + DC.MC_CALLER_METHOD_HASH + " having count(" + DC.MC_CALLER_METHOD_HASH + ") > 1";
        List<Object> multiImplMethodHashList = dbOperator.queryListOneColumn(sql, new Object[]{CallTypeEnum.CTE_ITF.getType(), JACGConstants.ENABLED});
        if (multiImplMethodHashList == null) {
            return false;
        }

        multiImplMethodHashSet = new HashSet<>(multiImplMethodHashList.size());
        for (Object multiImplMethodHash : multiImplMethodHashList) {
            multiImplMethodHashSet.add((String) multiImplMethodHash);
        }

        List<Object> multiChildrenMethodHashList = dbOperator.queryListOneColumn(sql, new Object[]{CallTypeEnum.CTE_SCC.getType(), JACGConstants.ENABLED});
        if (multiChildrenMethodHashList == null) {
            return false;
        }

        multiChildrenMethodHashSet = new HashSet<>(multiChildrenMethodHashList.size());
        for (Object multiChildrenMethodHash : multiChildrenMethodHashList) {
            multiChildrenMethodHashSet.add((String) multiChildrenMethodHash);
        }

        return true;
    }

    // 查询存在自定义数据的方法调用序号
    private boolean queryCallIdWithExtendedData() {
        // 只查询一次，不需要缓存
        String sql = "select distinct(" + DC.ED_CALL_ID + ") from " + JACGConstants.TABLE_PREFIX_EXTENDED_DATA + confInfo.getAppName();
        List<Object> callIdWithExtendedDataList = dbOperator.queryListOneColumn(sql, new Object[]{});
        if (callIdWithExtendedDataList == null) {
            return false;
        }
        if (callIdWithExtendedDataList.isEmpty()) {
            return true;
        }

        callIdWithExtendedDataSet = new HashSet<>(callIdWithExtendedDataList.size());
        for (Object callIdWithExtendedData : callIdWithExtendedDataList) {
            callIdWithExtendedDataSet.add((Integer) callIdWithExtendedData);
        }

        return true;
    }

    // 查询存在手工添加的自定义数据的调用方与被调用方完整方法
    private boolean queryCallFullMethodWithMAEDMap() {
        callFullMethodWithMAEDMap = new HashMap<>();

        // 只查询一次，不需要缓存
        String sql =
                "select distinct(" + DC.MAED_CALLER_FULL_METHOD + "), " + DC.MAED_CALLEE_FULL_METHOD + " from " + JACGConstants.TABLE_PREFIX_MANUAL_ADD_EXTENDED_DATA + confInfo.getAppName();
        List<Map<String, Object>> callFullMethodWithMAEDList = dbOperator.queryList(sql, new Object[]{});
        if (callFullMethodWithMAEDList == null) {
            return false;
        }

        if (callFullMethodWithMAEDList.isEmpty()) {
            return true;
        }

        for (Map<String, Object> mapInList : callFullMethodWithMAEDList) {
            String callerFullMethod = (String) mapInList.get(DC.MAED_CALLER_FULL_METHOD);
            String calleeFullMethod = (String) mapInList.get(DC.MAED_CALLEE_FULL_METHOD);

            Set<String> calleeFullMethodSet = callFullMethodWithMAEDMap.computeIfAbsent(callerFullMethod, k -> new HashSet<>());
            calleeFullMethodSet.add(calleeFullMethod);
        }

        return true;
    }

    // 生成需要执行的任务信息
    private List<CallerTaskInfo> genCallerTaskInfo() {
        List<CallerTaskInfo> callerTaskInfoList = new ArrayList<>(taskSet.size());
        Map<String, String> simpleClassNameMap = new HashMap<>(taskSet.size());

        for (String task : taskSet) {
            String left = task;
            int lineNumStart = JACGConstants.LINE_NUM_NONE;
            int lineNumEnd = JACGConstants.LINE_NUM_NONE;

            if (task.contains(JACGConstants.FLAG_SPACE)) {
                String[] array = task.split(JACGConstants.FLAG_SPACE);
                if (array.length != 2) {
                    logger.error("指定的类名+方法名非法，格式应为 [类名]:[方法名] [起始代码行号]-[结束代码行号] {}", task);
                    return null;
                }

                left = array[0];
                String right = array[1];
                String[] arrayRight = right.split(JACGConstants.FLAG_MINUS);
                if (arrayRight.length != 2) {
                    logger.error("指定的行号非法，格式应为 [起始代码行号]-[结束代码行号] {}", task);
                    return null;
                }

                if (!JACGUtil.isNumStr(arrayRight[0]) || !JACGUtil.isNumStr(arrayRight[1])) {
                    logger.error("指定的行号非法，应为数字 {}", task);
                    return null;
                }

                lineNumStart = Integer.parseInt(arrayRight[0]);
                lineNumEnd = Integer.parseInt(arrayRight[1]);
                if (lineNumStart <= 0 || lineNumEnd <= 0) {
                    logger.error("指定的行号非法，应为正整数 {}", task);
                    return null;
                }

                if (lineNumStart > lineNumEnd) {
                    logger.error("指定的行号非法，起始代码行号不能大于结束代码行号 {}", task);
                    return null;
                }
            }

            String[] arrayLeft = left.split(JACGConstants.FLAG_COLON);
            if (arrayLeft.length != 2) {
                logger.error("指定的类名+方法名非法，格式应为 [类名]:[方法名] {}", task);
                return null;
            }

            String callerClassName = arrayLeft[0];
            String callerMethodNameInTask = arrayLeft[1];

            if (StringUtils.isAnyBlank(callerClassName, callerMethodNameInTask)) {
                logger.error("指定的类名+方法名存在空值，格式应为 [类名]:[方法名] {}", task);
                return null;
            }

            String simpleClassName = simpleClassNameMap.get(callerClassName);
            if (simpleClassName == null) {
                // 获取简单类名
                simpleClassName = getSimpleClassName(callerClassName);
                if (simpleClassName == null) {
                    return null;
                }

                simpleClassNameMap.put(callerClassName, simpleClassName);
            }

            CallerTaskInfo callerTaskInfo = new CallerTaskInfo();
            callerTaskInfo.setCallerClassName(simpleClassName);
            callerTaskInfo.setCallerMethodName(callerMethodNameInTask);
            callerTaskInfo.setLineNumStart(lineNumStart);
            callerTaskInfo.setLineNumEnd(lineNumEnd);
            callerTaskInfo.setSaveDirPath(null);

            callerTaskInfoList.add(callerTaskInfo);
        }

        return callerTaskInfoList;
    }

    public boolean isSupportIgnore() {
        return supportIgnore;
    }

    public void setSupportIgnore(boolean supportIgnore) {
        this.supportIgnore = supportIgnore;
    }

    // 处理一条记录
    private boolean handleOneRecord(CallerTaskInfo callerTaskInfo) {
        String callerClassName = callerTaskInfo.getCallerClassName();
        String callerMethodNameInTask = callerTaskInfo.getCallerMethodName();
        int lineNumStart = callerTaskInfo.getLineNumStart();
        int lineNumEnd = callerTaskInfo.getLineNumEnd();

        // 从方法调用关系表查询指定的类是否存在
        if (!checkClassNameExists(callerClassName)) {
            // 生成空文件并返回成功
            return genEmptyFile(callerTaskInfo, callerClassName);
        }

        // 获取调用者完整类名
        String callerFullClassName = getCallerFullClassName(callerClassName);
        if (StringUtils.isBlank(callerFullClassName)) {
            // 生成空文件并返回成功
            return genEmptyFile(callerTaskInfo, callerClassName);
        }

        // 获取调用者最上层方法
        String sqlKey = JACGConstants.SQL_KEY_MC_QUERY_TOP_METHOD;
        String sql = sqlCacheMap.get(sqlKey);
        if (sql == null) {
            sql = "select distinct(" + DC.MC_CALLER_METHOD_HASH + ")," + DC.MC_CALLER_FULL_METHOD + " from " +
                    JACGConstants.TABLE_PREFIX_METHOD_CALL + confInfo.getAppName() + " where " + DC.MC_CALLER_CLASS_NAME +
                    "= ? and " + DC.MC_CALLER_FULL_METHOD + " like concat(?, '%')";
            cacheSql(sqlKey, sql);
        }

        String fullMethodPrefix = callerFullClassName + JACGConstants.FLAG_COLON + callerMethodNameInTask;

        List<Map<String, Object>> callerMethodList = dbOperator.queryList(sql, new Object[]{callerClassName, fullMethodPrefix});
        if (JACGUtil.isCollectionEmpty(callerMethodList)) {
            logger.warn("从方法调用关系表未找到指定的调用方法 {} {}", callerClassName, fullMethodPrefix);

            // 生成空文件并返回成功
            return genEmptyFile(callerTaskInfo, callerClassName);
        }

        boolean findMethod = false;
        String callerMethodHash = null;
        String callerFullMethod = null;

        // 遍历找到的方法
        for (Map<String, Object> callerMethodMap : callerMethodList) {
            String currentCallerFullMethod = (String) callerMethodMap.get(DC.MC_CALLER_FULL_METHOD);
            String currentMethodWithArgs = JACGUtil.getMethodWithArgs(currentCallerFullMethod);

            // 当方法名为以下前缀时，忽略
            if (isEntryMethodIgnoredWithPrefixByMethodNameWithArgs(currentMethodWithArgs)) {
                continue;
            }

            // 找到一个方法
            if (findMethod) {
                logger.error("通过方法前缀找到多于一个入口方法 {} {} ，请指定完整方法名，或在文件 {} 中指定排除", callerFullMethod, currentCallerFullMethod,
                        JACGConstants.FILE_OUT_GRAPH_FOR_CALLER_ENTRY_METHOD_IGNORE_PREFIX);
                return false;
            }

            findMethod = true;
            callerMethodHash = (String) callerMethodMap.get(DC.MC_CALLER_METHOD_HASH);
            callerFullMethod = currentCallerFullMethod;
        }

        if (StringUtils.isBlank(callerMethodHash)) {
            logger.error("未找到指定的入口方法{} {}", callerClassName, callerMethodNameInTask);
            return false;
        }

        logger.debug("找到入口方法 {} {}", callerMethodHash, callerFullMethod);

        // 获取当前实际的方法名，而不是使用文件中指定的方法名，文件中指定的方法名可能包含参数，会很长，不可控
        String callerMethodName = JACGUtil.getOnlyMethodName(callerFullMethod);
        String safeMethodName = JACGUtil.getSafeMethodName(callerMethodName);

        // 确定当前方法对应输出文件路径，格式: 配置文件中指定的类名（简单类名或全名）+方法名+方法名hash.txt
        StringBuilder sbOutputFilePath = new StringBuilder(outputDirPrefix).append(File.separator);
        if (callerTaskInfo.getSaveDirPath() != null) {
            sbOutputFilePath.append(callerTaskInfo.getSaveDirPath()).append(File.separator);

            // 创建对应目录
            if (!FileUtil.isDirectoryExists(sbOutputFilePath.toString())) {
                return false;
            }
        }
        sbOutputFilePath.append(callerClassName).append(JACGConstants.FLAG_AT).append(safeMethodName).append(JACGConstants.FLAG_AT).append(callerMethodHash);
        if (lineNumStart != JACGConstants.LINE_NUM_NONE && lineNumEnd != JACGConstants.LINE_NUM_NONE) {
            sbOutputFilePath.append(JACGConstants.FLAG_AT).append(lineNumStart).append(JACGConstants.FLAG_MINUS).append(lineNumEnd);
        }
        sbOutputFilePath.append(JACGConstants.EXT_TXT);
        String outputFileName = sbOutputFilePath.toString();
        logger.info("当前输出文件名 {}", outputFileName);

        try (BufferedWriter out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(outputFileName), StandardCharsets.UTF_8))) {
            if (confInfo.isWriteConf()) {
                // 在结果文件中写入配置信息
                out.write(confInfo.toString());
                out.write(JACGConstants.NEW_LINE);
                out.write("supportIgnore: " + isSupportIgnore());
                out.write(JACGConstants.NEW_LINE);
            }

            // 在文件第1行写入当前方法的完整信息
            out.write(callerFullMethod);
            out.write(JACGConstants.NEW_LINE);

            // 确定写入输出文件的当前被调用方法信息
            String calleeInfo = chooseCalleeInfo(callerFullMethod, callerFullClassName, callerMethodName, callerClassName);

            // 第2行写入当前方法的信息
            out.write(genOutputPrefix(0));
            out.write(calleeInfo);
            // 写入方法注解
            String methodAnnotations = methodAnnotationsMap.get(callerMethodHash);
            if (methodAnnotations != null) {
                out.write(methodAnnotations);
            }

            out.write(JACGConstants.NEW_LINE);

            // 根据指定的调用者方法HASH，查找所有被调用的方法信息
            return genAllGraph4Caller(callerMethodHash, out, callerFullMethod, lineNumStart, lineNumEnd);
        } catch (Exception e) {
            logger.error("error ", e);
            return false;
        }
    }

    // 生成空文件
    private boolean genEmptyFile(CallerTaskInfo callerTaskInfo, String callerClassName) {
        // 生成空文件并返回成功
        String emptyFilePath = outputDirPrefix + File.separator;
        if (callerTaskInfo.getSaveDirPath() != null) {
            emptyFilePath = emptyFilePath + callerTaskInfo.getSaveDirPath() + File.separator;

            // 创建对应目录
            if (!FileUtil.isDirectoryExists(emptyFilePath)) {
                return false;
            }
        }

        emptyFilePath = emptyFilePath + callerClassName + JACGConstants.EMPTY_FILE_FLAG + JACGConstants.EXT_TXT;

        // 创建文件
        return FileUtil.createNewFile(emptyFilePath);
    }

    /**
     * 根据指定的调用者方法HASH，查找所有被调用方法信息
     *
     * @param callerMethodHash
     * @param out
     * @param callerFullMethod 仅代表调用当前方法时的调用者方法，不代表以下while循环中每次处理到的调用者方法
     * @param lineNumStart
     * @param lineNumEnd
     * @return
     */
    protected boolean genAllGraph4Caller(String callerMethodHash, BufferedWriter out, String callerFullMethod, int lineNumStart, int lineNumEnd) throws IOException {

        // 通过List记录当前遍历到的节点信息（当作不定长数组使用）
        List<TmpNode4Caller> node4CallerList = new ArrayList<>();

        // 初始加入最上层节点，id设为0（方法调用关系表最小id为1）
        TmpNode4Caller headNode = TmpNode4Caller.genNode(callerMethodHash, JACGConstants.METHOD_CALL_ID_START);
        node4CallerList.add(headNode);

        // 记录当前处理的节点层级
        int currentNodeLevel = 0;

        // 输出结果行数
        int outputLineNum = 0;

        // 在一个调用方法中出现多次的被调用方法（包含自定义数据），是否需要忽略
        boolean ignoreDupCalleeInOneCaller = confInfo.isIgnoreDupCalleeInOneCaller();

        // 记录各个层级的调用方法中有被调用过的方法（包含方法注解、自定义数据）
        Map<Integer, Set<String>> recordedCalleeMap = null;
        if (ignoreDupCalleeInOneCaller) {
            recordedCalleeMap = new HashMap<>();
            // 为第0层的调用者添加Set，不能在以下while循环的if (currentNodeLevel == 0)中添加，因为会执行多次
            recordedCalleeMap.put(0, new HashSet<>());
        }

        while (true) {
            int currentLineNumStart = JACGConstants.LINE_NUM_NONE;
            int currentLineNumEnd = JACGConstants.LINE_NUM_NONE;
            if (currentNodeLevel == 0) {
                currentLineNumStart = lineNumStart;
                currentLineNumEnd = lineNumEnd;
            }

            TmpNode4Caller currentNode = node4CallerList.get(currentNodeLevel);

            // 查询当前节点的一个下层被调用方法
            Map<String, Object> calleeMethodMap = queryOneCalleeMethod(currentNode, currentLineNumStart, currentLineNumEnd);
            if (calleeMethodMap == null) {
                // 查询失败
                return false;
            }

            if (calleeMethodMap.isEmpty()) {
                // 未查询到记录
                if (currentNodeLevel <= 0) {
                    // 当前处理的节点为最上层节点，结束循环
                    return true;
                }

                // 当前处理的节点不是最上层节点，返回上一层处理
                if (ignoreDupCalleeInOneCaller) {
                    // 清空不再使用的下一层Set
                    recordedCalleeMap.put(currentNodeLevel, null);
                }
                currentNodeLevel--;
                continue;
            }
            // 查询到记录
            outputLineNum++;
            if (outputLineNum % JACGConstants.NOTICE_LINE_NUM == 0) {
                logger.info("记录数达到 {} {}", outputLineNum, callerFullMethod);
            }

            int currentMethodCallId = (Integer) calleeMethodMap.get(DC.MC_ID);
            int enabled = (Integer) calleeMethodMap.get(DC.MC_ENABLED);

            // 判断是否需要忽略
            if ((isSupportIgnore() && ignoreCurrentMethod(calleeMethodMap)) ||
                    enabled != JACGConstants.ENABLED) {
                // 当前记录需要忽略
                // 更新当前处理节点的id
                node4CallerList.get(currentNodeLevel).setCurrentCalleeMethodId(currentMethodCallId);

                if (enabled != JACGConstants.ENABLED) {
                    String callType = (String) calleeMethodMap.get(DC.MC_CALL_TYPE);

                    // 记录被禁用的方法调用
                    recordDisabledMethodCall(currentMethodCallId, callType);
                }

                continue;
            }

            // 当前记录需要处理
            String currentCalleeMethodHash = (String) calleeMethodMap.get(DC.MC_CALLEE_METHOD_HASH);

            // 判断被调用的方法是否为存在多个实现类的接口或父类方法
            CallTypeEnum multiImplMethodCallType = null;
            if (multiImplMethodHashSet.contains(currentCalleeMethodHash)) {
                multiImplMethodCallType = CallTypeEnum.CTE_ITF;
            } else if (multiChildrenMethodHashSet.contains(currentCalleeMethodHash)) {
                multiImplMethodCallType = CallTypeEnum.CTE_SCC;
            }

            // 获取被调用方法信息（包含方法注解信息、自定义数据）
            String calleeInfo = getCalleeInfo(calleeMethodMap, currentCalleeMethodHash, currentMethodCallId, multiImplMethodCallType);
            if (calleeInfo == null) {
                return false;
            }

            if (ignoreDupCalleeInOneCaller) {
                Set<String> callerRecordedCalleeSet = recordedCalleeMap.get(currentNodeLevel);
                if (callerRecordedCalleeSet.contains(calleeInfo)) {
                    // 当前被调用方法在调用方法中已被调用过，忽略
                    logger.debug("忽略一个方法中被调用多次的方法 {} {}", currentNodeLevel, calleeInfo);

                    // 更新当前处理节点的id
                    node4CallerList.get(currentNodeLevel).setCurrentCalleeMethodId(currentMethodCallId);
                    continue;
                }

                // 当前被调用方法在调用方法中未被调用过，记录
                callerRecordedCalleeSet.add(calleeInfo);
            }

            // 检查是否出现循环调用
            int back2Level = checkCycleCall(node4CallerList, currentNodeLevel, currentCalleeMethodHash);

            // 记录被调用方法信息
            if (!recordCalleeInfo(calleeMethodMap, currentNodeLevel, back2Level, out, currentMethodCallId, calleeInfo)) {
                return false;
            }

            if (multiImplMethodCallType != null) {
                // 被调用的方法是否为存在多个实现类的接口或父类方法时，不再往下找被调用的方法
                // 更新当前处理节点的id
                node4CallerList.get(currentNodeLevel).setCurrentCalleeMethodId(currentMethodCallId);
                continue;
            }

            if (back2Level != JACGConstants.NO_CYCLE_CALL_FLAG) {
                logger.info("找到循环调用 {} [{}]", currentCalleeMethodHash, back2Level);

                // 将当前处理的层级指定到循环调用的节点
                currentNodeLevel = back2Level;
                continue;
            }

            // 更新当前处理节点的id
            node4CallerList.get(currentNodeLevel).setCurrentCalleeMethodId(currentMethodCallId);

            // 继续下一层处理
            currentNodeLevel++;
            if (ignoreDupCalleeInOneCaller) {
                // 开始处理下一层，设置对应的Set
                recordedCalleeMap.put(currentNodeLevel, new HashSet<>());
            }

            // 获取下一层节点
            if (currentNodeLevel + 1 > node4CallerList.size()) {
                // 下一层节点不存在，则需要添加，id设为0（方法调用关系表最小id为1）
                TmpNode4Caller nextNode = TmpNode4Caller.genNode(currentCalleeMethodHash, JACGConstants.METHOD_CALL_ID_START);
                node4CallerList.add(nextNode);
            } else {
                // 下一层节点已存在，则修改值
                TmpNode4Caller nextNode = node4CallerList.get(currentNodeLevel);
                nextNode.setCurrentCalleeMethodHash(currentCalleeMethodHash);
                nextNode.setCurrentCalleeMethodId(JACGConstants.METHOD_CALL_ID_START);
            }
        }
    }

    /**
     * 检查是否出现循环调用
     *
     * @param node4CallerList
     * @param currentNodeLevel        不需要遍历整个List，因为后面的元素可能当前无效
     * @param currentCalleeMethodHash
     * @return -1: 未出现循环调用，非-1: 出现循环依赖，值为发生循环调用的层级
     */
    private int checkCycleCall(List<TmpNode4Caller> node4CallerList, int currentNodeLevel, String currentCalleeMethodHash) {
        for (int i = currentNodeLevel; i >= 0; i--) {
            if (currentCalleeMethodHash.equals(node4CallerList.get(i).getCurrentCalleeMethodHash())) {
                return i;
            }
        }
        return JACGConstants.NO_CYCLE_CALL_FLAG;
    }

    // 查询当前节点的一个下层被调用方法
    private Map<String, Object> queryOneCalleeMethod(TmpNode4Caller node, int currentLineNumStart, int currentLineNumEnd) {
        // 确定通过被调用方法进行查询使用的SQL语句
        String sql = chooseQueryCalleeMethodSql(currentLineNumStart, currentLineNumEnd);

        List<Object> argList = new ArrayList<>(4);
        argList.add(node.getCurrentCalleeMethodHash());
        argList.add(node.getCurrentCalleeMethodId());
        if (currentLineNumStart != JACGConstants.LINE_NUM_NONE && currentLineNumEnd != JACGConstants.LINE_NUM_NONE) {
            argList.add(currentLineNumStart);
            argList.add(currentLineNumEnd);
        }

        List<Map<String, Object>> list = dbOperator.queryList(sql, argList.toArray());
        if (list == null) {
            // 查询失败
            return null;
        }

        if (list.isEmpty()) {
            // 查询不到结果时，返回空Map
            return new HashMap<>(0);
        }

        return list.get(0);
    }

    // 确定通过被调用方法进行查询使用的SQL语句
    protected String chooseQueryCalleeMethodSql(int currentLineNumStart, int currentLineNumEnd) {
        String sqlKey = JACGConstants.SQL_KEY_MC_QUERY_ONE_CALLEE;
        if (currentLineNumStart != JACGConstants.LINE_NUM_NONE && currentLineNumEnd != JACGConstants.LINE_NUM_NONE) {
            sqlKey = JACGConstants.SQL_KEY_MC_QUERY_ONE_CALLEE_CHECK_LINE_NUM;
        }

        String sql = sqlCacheMap.get(sqlKey);
        if (sql == null) {
            // 确定查询被调用关系时所需字段
            String selectMethodColumns = chooseSelectMethodColumns();

            StringBuilder sbSql = new StringBuilder("select ").append(selectMethodColumns).append(" from ")
                    .append(JACGConstants.TABLE_PREFIX_METHOD_CALL).append(confInfo.getAppName()).append(" where ")
                    .append(DC.MC_CALLER_METHOD_HASH).append(" = ? and ").append(DC.MC_ID).append(" > ?");
            if (currentLineNumStart != JACGConstants.LINE_NUM_NONE && currentLineNumEnd != JACGConstants.LINE_NUM_NONE) {
                sbSql.append(" and ").append(DC.MC_CALLER_LINE_NUM).append(" >= ? and ").append(DC.MC_CALLER_LINE_NUM).append(" <= ?");
            }
            sbSql.append(" order by ").append(DC.MC_ID).append(" limit 1");
            sql = sbSql.toString();
            cacheSql(sqlKey, sql);
        }
        return sql;
    }

    // 判断当前找到的被调用方法是否需要处理
    private boolean ignoreCurrentMethod(Map<String, Object> methodMapByCaller) {
        String callType = (String) methodMapByCaller.get(DC.MC_CALL_TYPE);
        String calleeFullMethod = (String) methodMapByCaller.get(DC.MC_CALLEE_FULL_METHOD);

        // 当完整方法（类名+方法名+参数）为以下前缀时，忽略
        if (isIgnoredFullMethodWithPrefixByFullMethod(calleeFullMethod)) {
            return true;
        }

        String calleeFullClassName = JACGUtil.getFullClassNameFromMethod(calleeFullMethod);

        // 根据类名特定关键字判断是否需要忽略
        if (isIgnoredClassWithKeywordByFullClass(calleeFullClassName)) {
            return true;
        }

        String calleeMethodNameWithArgs = JACGUtil.getMethodWithArgs(calleeFullMethod);

        /*
            根据方法名前缀判断是否需要忽略，使用包含参数的方法名进行比较
            若当前调用类型为Runnable/Callable实现类子类构造函数调用run()方法，则不判断方法名前缀是否需要忽略（<init> -> run()，可能会被指定为忽略）
         */
        if (!StringUtils.equalsAny(callType, CallTypeEnum.CTE_RIR.getType(), CallTypeEnum.CTE_CIC.getType())
                && isIgnoredMethodWithPrefixByMethodName(calleeMethodNameWithArgs)) {
            return true;
        }

        return false;
    }

    // 获取被调用方法信息（包含方法注解信息、自定义数据）
    protected String getCalleeInfo(Map<String, Object> calleeMethodMap, String currentCalleeMethodHash, int currentMethodCallId, CallTypeEnum multiImplMethodCallType) {
        StringBuilder calleeInfo = new StringBuilder();

        String callerFullMethod = (String) calleeMethodMap.get(DC.MC_CALLER_FULL_METHOD);
        String calleeFullMethod = (String) calleeMethodMap.get(DC.MC_CALLEE_FULL_METHOD);

        if (confInfo.getCallGraphOutputDetail().equals(JACGConstants.CONFIG_OUTPUT_DETAIL_1)) {
            // # 1: 展示 完整类名+方法名+方法参数
            calleeInfo.append(calleeFullMethod);
        } else if (confInfo.getCallGraphOutputDetail().equals(JACGConstants.CONFIG_OUTPUT_DETAIL_2)) {
            // # 2: 展示 完整类名+方法名
            String calleeFullClassName = JACGUtil.getFullClassNameFromMethod(calleeFullMethod);
            String calleeMethodName = JACGUtil.getOnlyMethodName(calleeFullMethod);

            calleeInfo.append(calleeFullClassName)
                    .append(JACGConstants.FLAG_COLON)
                    .append(calleeMethodName);
        } else {
            // # 3: 展示 简单类名（对于同名类展示完整类名）+方法名
            String calleeMethodName = JACGUtil.getOnlyMethodName(calleeFullMethod);

            calleeInfo.append(calleeMethodMap.get(DC.MC_CALLEE_CLASS_NAME))
                    .append(JACGConstants.FLAG_COLON)
                    .append(calleeMethodName);
        }

        // 添加方法注解信息
        if (confInfo.isShowMethodAnnotation()) {
            String methodAnnotations = methodAnnotationsMap.get(currentCalleeMethodHash);
            if (methodAnnotations != null) {
                calleeInfo.append(methodAnnotations);
            }
        }

        if (multiImplMethodCallType != null) {
            // 对存在多个实现类的接口或父类方法进行处理
            if (!handleMultiImplMethod(calleeInfo, currentMethodCallId, currentCalleeMethodHash, multiImplMethodCallType)) {
                return null;
            }
            return calleeInfo.toString();
        }

        // 添加自定义数据
        if (!addExtendedData(currentMethodCallId, calleeInfo, currentCalleeMethodHash, callerFullMethod, calleeFullMethod)) {
            return null;
        }

        return calleeInfo.toString();
    }

    // 对存在多个实现类的接口或父类方法进行处理
    private boolean handleMultiImplMethod(StringBuilder calleeInfo, int currentMethodCallId, String currentCalleeMethodHash, CallTypeEnum multiImplMethodCallType) {
        logger.info("对存在多个实现类的接口或父类方法进行处理 {} {} {}", currentMethodCallId, currentCalleeMethodHash, multiImplMethodCallType);

        // 查询被调用方法类名与方法名
        String sqlKey = JACGConstants.SQL_KEY_MC_QUERY_CALLEE_BY_ID;
        String sql = sqlCacheMap.get(sqlKey);
        if (sql == null) {
            sql = "select " + StringUtils.joinWith(JACGConstants.FLAG_COMMA_WITH_SPACE, DC.MC_CALLEE_CLASS_NAME, DC.MC_CALLEE_METHOD_NAME) + " from " + JACGConstants.TABLE_PREFIX_METHOD_CALL + confInfo.getAppName() +
                    " where " + DC.MC_ID + " = ?";
            cacheSql(sqlKey, sql);
        }

        Map<String, Object> calleeWithMultiImplInfoMap = dbOperator.queryOneRow(sql, new Object[]{currentMethodCallId});
        if (JACGUtil.isMapEmpty(calleeWithMultiImplInfoMap)) {
            logger.error("根据调用序号查找对应被调用方法信息失败 {}", currentMethodCallId);
            return false;
        }

        String safeMethodName = JACGUtil.getSafeMethodName((String) calleeWithMultiImplInfoMap.get(DC.MC_CALLEE_METHOD_NAME));
        // 写入结果文件的数据格式：[完整或简单类名]@[方法名]@[方法HASH]
        String calleeWithMultiImplName = calleeWithMultiImplInfoMap.get(DC.MC_CALLEE_CLASS_NAME) + JACGConstants.FLAG_AT + safeMethodName +
                JACGConstants.FLAG_AT + currentCalleeMethodHash;

        // 将被调用方法信息作为自定义数据加入被调用方法信息中
        addExtendedData2CalleeInfo(true, JACGConstants.DATA_TYPE_JUMP_MULTI_IMPL, calleeWithMultiImplName, calleeInfo);

        // 记录本次执行时查询到存在多个实现类的接口或父类方法信息
        if (allFoundMultiImplMethodMap.putIfAbsent(currentCalleeMethodHash, Boolean.TRUE) == null) {
            // 仅记录未被处理过的方法信息
            MultiImplMethodInfo multiImplMethodInfo = new MultiImplMethodInfo();
            multiImplMethodInfo.setMultiImplMethodCallType(multiImplMethodCallType);
            multiImplMethodInfo.setDirPath(calleeWithMultiImplName);

            currentFoundMultiImplMethodMap.put(currentCalleeMethodHash, multiImplMethodInfo);
        }

        return true;
    }

    // 记录被调用方法信息
    protected boolean recordCalleeInfo(Map<String, Object> calleeMethodMap, int currentNodeLevel, int back2Level, BufferedWriter out, int currentMethodCallId, String calleeInfo) throws IOException {
        // 生成输出文件前缀，包含了当前方法的调用层级
        String prefix = genOutputPrefix(currentNodeLevel + 1);

        // 写入前缀：    "[2]#    "
        out.write(prefix);

        // 写入调用者行号信息：   "[Service1Impl:29]	"
        if (confInfo.isShowCallerLineNum()) {
            // 显示调用者代码行号
            String callerLineNumber = JACGConstants.FLAG_LEFT_PARENTHESES +
                    calleeMethodMap.get(DC.MC_CALLER_CLASS_NAME) +
                    JACGConstants.FLAG_COLON +
                    calleeMethodMap.get(DC.MC_CALLER_LINE_NUM) +
                    JACGConstants.FLAG_RIGHT_PARENTHESES +
                    JACGConstants.FLAG_TAB;
            out.write(callerLineNumber);
        }

        // 写入被调用者信息（包含方法注解信息、自定义数据）：    test.example.service.impl.Service1Impl:test1
        out.write(calleeInfo);

        // 写入循环调用标志
        if (back2Level != JACGConstants.NO_CYCLE_CALL_FLAG) {
            out.write(String.format(JACGConstants.CALL_FLAG_CYCLE, back2Level));
        }

        // 写入换行符
        out.write(JACGConstants.NEW_LINE);

        String callType = (String) calleeMethodMap.get(DC.MC_CALL_TYPE);
        // 记录可能出现一对多的方法调用
        return recordMethodCallMayBeMulti(currentMethodCallId, callType);
    }

    // 添加自定义数据
    private boolean addExtendedData(int currentMethodCallId, StringBuilder calleeInfo, String calleeMethodHash, String callerFullMethod, String calleeFullMethod) {
        // 处理数据库中手工添加的自定义数据
        ExtendedDataResultEnum result = handleManualAddExtendedDataInDb(currentMethodCallId, calleeMethodHash, callerFullMethod, calleeFullMethod, calleeInfo);
        if (result == ExtendedDataResultEnum.EDRE_FAIL) {
            // 处理失败
            return false;
        }
        if (result == ExtendedDataResultEnum.EDRE_SUCCESS) {
            // 处理了手工添加的自定义数据，返回成功
            return true;
        }

        // 数据库中不存在手工添加的自定义数据
        // 调用自定义处理类添加自定义数据
        result = addExtendedDataByExtensions(currentMethodCallId, calleeMethodHash, callerFullMethod, calleeFullMethod, calleeInfo);
        if (result == ExtendedDataResultEnum.EDRE_FAIL) {
            // 处理失败
            return false;
        }
        if (result == ExtendedDataResultEnum.EDRE_SUCCESS) {
            // 处理了手工添加的自定义数据，返回成功
            return true;
        }

        // 自定义处理类未添加数据
        // 继续处理程序识别的自定义数据
        if (callIdWithExtendedDataSet == null || !callIdWithExtendedDataSet.contains(currentMethodCallId)) {
            // 不存在程序识别的自定义数据，返回成功
            return true;
        }

        // 存在程序识别的自定义数据，从数据库查询
        String sqlKey = JACGConstants.SQL_KEY_ED_QUERY_EXTENDED_DATA;
        String sql = sqlCacheMap.get(sqlKey);
        if (sql == null) {
            String columns = StringUtils.joinWith(JACGConstants.FLAG_COMMA_WITH_SPACE, DC.ED_DATA_TYPE, DC.ED_DATA_VALUE);
            sql = "select " + columns + " from " + JACGConstants.TABLE_PREFIX_EXTENDED_DATA + confInfo.getAppName() +
                    " where " + DC.ED_CALL_ID + " = ?";
            cacheSql(sqlKey, sql);
        }

        Map<String, Object> extendedDataMap = dbOperator.queryOneRow(sql, new Object[]{currentMethodCallId});
        if (JACGUtil.isMapEmpty(extendedDataMap)) {
            logger.error("查询自定义数据不存在 {}", currentMethodCallId);
            return false;
        }

        // 将自定义数据加入被调用方法信息中
        return addExtendedData2CalleeInfo(false, (String) extendedDataMap.get(DC.ED_DATA_TYPE), (String) extendedDataMap.get(DC.ED_DATA_VALUE), calleeInfo);
    }

    /**
     * 将自定义数据加入被调用方法信息中
     *
     * @param addJumpMultiImpl 是否为处理存在多个实现类的接口或父类方法
     * @param dataType         自定义数据类型
     * @param dataValue        自定义数据值
     * @param calleeInfo       被调用方法信息
     */
    private boolean addExtendedData2CalleeInfo(boolean addJumpMultiImpl, String dataType, String dataValue, StringBuilder calleeInfo) {
        if (!addJumpMultiImpl && JACGConstants.DATA_TYPE_JUMP_MULTI_IMPL.equals(dataType)) {
            logger.error("当前的数据类型不允许使用，请使用其他值 {}", dataType);
            return false;
        }

        if (dataType.contains(JACGConstants.FLAG_AT)) {
            logger.error("当前的数据类型不允许使用当前标志，请使用其他值 {} {}", JACGConstants.FLAG_AT, dataType);
            return false;
        }

        // 对自定义数据进行补充
        String dataValueAfterSupplement = supplementExtendedData(dataType, dataValue);

        calleeInfo.append(JACGConstants.CALL_FLAG_EXTENDED_DATA)
                .append(dataType)
                .append(JACGConstants.FLAG_AT)
                .append(dataValueAfterSupplement);

        return true;
    }

    /**
     * 处理数据库中手工添加的自定义数据
     *
     * @param currentMethodCallId
     * @param calleeMethodHash
     * @param callerFullMethod
     * @param calleeFullMethod
     * @param calleeInfo
     * @return TRUE: 处理成功, FALSE: 不存在手工添加的自定义数据, null: 处理失败
     */
    private ExtendedDataResultEnum handleManualAddExtendedDataInDb(int currentMethodCallId, String calleeMethodHash, String callerFullMethod, String calleeFullMethod,
                                                                   StringBuilder calleeInfo) {
        // 根据缓存数据判断当前调用者及被调用者方法是否存在手工添加的自定义数据
        Set<String> calleeFullMethodSet = callFullMethodWithMAEDMap.get(callerFullMethod);
        if (calleeFullMethodSet == null || !calleeFullMethodSet.contains(calleeFullMethod)) {
            // 匹配调用者方法查询不存在时，再使用*查询
            calleeFullMethodSet = callFullMethodWithMAEDMap.get(JACGConstants.SQL_VALUE_MAED_CALLER_FULL_METHOD_ALL);
            if (calleeFullMethodSet == null || !calleeFullMethodSet.contains(calleeFullMethod)) {
                // 当前调用者及被调用者方法不存在手工添加的自定义数据
                return ExtendedDataResultEnum.EDRE_NONE;
            }
        }

        // 查询当前调用者中，被调用者方法出现的序号
        long calleeSeqInCaller = getCalleeSeqInCaller(calleeMethodHash, callerFullMethod, currentMethodCallId);
        if (calleeSeqInCaller == 0) {
            return ExtendedDataResultEnum.EDRE_FAIL;
        }

        // 查询当前调用关系手工添加的自定义数据
        List<Map<String, Object>> list4MAED = queryMAED(callerFullMethod, calleeFullMethod, calleeSeqInCaller);
        if (list4MAED == null) {
            logger.error("查询当前调用关系手工添加的自定义数据失败 {} {} {}", callerFullMethod, calleeFullMethod, calleeSeqInCaller);
            return ExtendedDataResultEnum.EDRE_FAIL;
        }
        if (list4MAED.isEmpty()) {
            // 当前调用关系不存在手工添加的自定义数据
            return ExtendedDataResultEnum.EDRE_NONE;
        }

        if (list4MAED.size() > 1) {
            logger.error("当前调用关系存在多条手工添加的自定义数据，请仅保留一条 {} {} {}", callerFullMethod, calleeFullMethod, calleeSeqInCaller);
            return ExtendedDataResultEnum.EDRE_FAIL;
        }

        Map<String, Object> map4MAED = list4MAED.get(0);

        // 将自定义数据加入被调用方法信息中
        if (!addExtendedData2CalleeInfo(false, (String) map4MAED.get(DC.MAED_DATA_TYPE), (String) map4MAED.get(DC.MAED_DATA_VALUE), calleeInfo)) {
            return ExtendedDataResultEnum.EDRE_FAIL;
        }

        return ExtendedDataResultEnum.EDRE_SUCCESS;
    }

    /**
     * 查询当前被调用方法在调用方法中的序号
     *
     * @param calleeMethodHash    被调用完整方法HASH
     * @param callerFullMethod    调用完整方法
     * @param currentMethodCallId 当前方法调用id
     * @return 0: 查找失败；非0: 查找成功
     */
    private long getCalleeSeqInCaller(String calleeMethodHash, String callerFullMethod, int currentMethodCallId) {
        String sqlKey = JACGConstants.SQL_KEY_MC_QUERY_CALLEE_SEQ_IN_CALLER;
        String sql4CalleeSeq = sqlCacheMap.get(sqlKey);
        if (sql4CalleeSeq == null) {
            sql4CalleeSeq = "select count(*) from " + JACGConstants.TABLE_PREFIX_METHOD_CALL + confInfo.getAppName() +
                    " where " + DC.MC_CALLEE_METHOD_HASH + " = ? and " + DC.MC_CALLER_FULL_METHOD + " = ? and " + DC.MC_ID + " <= ? order by " + DC.MC_ID;
            cacheSql(sqlKey, sql4CalleeSeq);
        }

        List<Object> list4CalleeSeq = dbOperator.queryListOneColumn(sql4CalleeSeq, new Object[]{calleeMethodHash, callerFullMethod, currentMethodCallId});
        if (JACGUtil.isCollectionEmpty(list4CalleeSeq)) {
            logger.error("查询当前调用者中，被调用者方法出现的序号失败 {} {} {}", calleeMethodHash, callerFullMethod, currentMethodCallId);
            return 0;
        }

        long calleeSeqInCaller = (long) list4CalleeSeq.get(0);
        if (calleeSeqInCaller <= 0) {
            logger.error("查询当前调用者中，被调用者方法出现的序号失败2 {} {} {}", calleeMethodHash, callerFullMethod, currentMethodCallId);
            return 0;
        }

        return calleeSeqInCaller;
    }

    /**
     * 调用自定义处理类添加自定义数据
     *
     * @param currentMethodCallId
     * @param calleeMethodHash
     * @param callerFullMethod
     * @param calleeFullMethod
     * @param calleeInfo
     * @return
     */
    private ExtendedDataResultEnum addExtendedDataByExtensions(int currentMethodCallId, String calleeMethodHash, String callerFullMethod, String calleeFullMethod,
                                                               StringBuilder calleeInfo) {
        if (JACGUtil.isCollectionEmpty(extendedDataAddExtList)) {
            // 用于添加自定义数据的处理类为空，返回不存在自定义数据
            return ExtendedDataResultEnum.EDRE_NONE;
        }

        for (ExtendedDataAddInterface extendedDataAddExt : extendedDataAddExtList) {
            // 判断当前方法调用是否需要由自定义处理类处理
            if (!extendedDataAddExt.checkMethodCall(callerFullMethod, calleeFullMethod)) {
                continue;
            }

            // 查询当前调用者中，被调用者方法出现的序号
            long calleeSeqInCaller = getCalleeSeqInCaller(calleeMethodHash, callerFullMethod, currentMethodCallId);
            if (calleeSeqInCaller == 0) {
                // 获取序号失败
                return ExtendedDataResultEnum.EDRE_FAIL;
            }

            BaseExtendedData extendedData = extendedDataAddExt.getExtendedData(callerFullMethod, calleeFullMethod, calleeSeqInCaller);
            if (extendedData != null) {
                if (extendedData.getDataType() == null || extendedData.getDataValue() == null) {
                    logger.error("返回的自定义数据存在字段为空 {}", extendedData);
                    return ExtendedDataResultEnum.EDRE_FAIL;
                }

                // 将自定义数据加入被调用方法信息中
                if (!addExtendedData2CalleeInfo(false, extendedData.getDataType(), extendedData.getDataValue(), calleeInfo)) {
                    return ExtendedDataResultEnum.EDRE_FAIL;
                }

                return ExtendedDataResultEnum.EDRE_SUCCESS;
            }
        }

        // 返回不存在自定义数据
        return ExtendedDataResultEnum.EDRE_NONE;
    }

    // 查询手工添加的自定义数据
    private List<Map<String, Object>> queryMAED(String callerFullMethod, String calleeFullMethod, long calleeSeqInCaller) {
        // 查询当前调用关系手工添加的自定义数据，先指定调用者进行查询
        String sqlKey4MAED = JACGConstants.SQL_KEY_MAED_QUERY;
        String sql4MAED = sqlCacheMap.get(sqlKey4MAED);
        if (sql4MAED == null) {
            String columns = StringUtils.joinWith(JACGConstants.FLAG_COMMA_WITH_SPACE, DC.MAED_DATA_TYPE, DC.MAED_DATA_VALUE);
            sql4MAED = "select " + columns + " from " + JACGConstants.TABLE_PREFIX_MANUAL_ADD_EXTENDED_DATA + confInfo.getAppName() +
                    " where " + DC.MAED_CALLER_FULL_METHOD + " = ? and " + DC.MAED_CALLEE_FULL_METHOD + " = ? and " + DC.MAED_CALLEE_SEQ_IN_CALLER + " = ?";
            cacheSql(sqlKey4MAED, sql4MAED);
        }

        List<Map<String, Object>> list4MAED = dbOperator.queryList(sql4MAED, new Object[]{callerFullMethod, calleeFullMethod, calleeSeqInCaller});

        if (list4MAED == null || !list4MAED.isEmpty()) {
            // 等于null，代表查询失败，返回；查询结果非空，返回；只有查询成功且结果为空时，才继续
            return list4MAED;
        }

        // 查询当前调用关系手工添加的自定义数据，再不限制调用者进行查询
        String sqlKey4MAEDIgnoreCaller = JACGConstants.SQL_KEY_MAED_QUERY_IGNORE_CALLER;
        String sql4MAEDIgnoreCaller = sqlCacheMap.get(sqlKey4MAEDIgnoreCaller);
        if (sql4MAEDIgnoreCaller == null) {
            String columns = StringUtils.joinWith(JACGConstants.FLAG_COMMA_WITH_SPACE, DC.MAED_DATA_TYPE, DC.MAED_DATA_VALUE);
            sql4MAEDIgnoreCaller = "select " + columns + " from " + JACGConstants.TABLE_PREFIX_MANUAL_ADD_EXTENDED_DATA + confInfo.getAppName() +
                    " where " + DC.MAED_CALLER_FULL_METHOD + " = ? and " + DC.MAED_CALLEE_FULL_METHOD + " = ?";
            cacheSql(sqlKey4MAEDIgnoreCaller, sql4MAEDIgnoreCaller);
        }

        return dbOperator.queryList(sql4MAEDIgnoreCaller, new Object[]{JACGConstants.SQL_VALUE_MAED_CALLER_FULL_METHOD_ALL, calleeFullMethod});
    }

    // 对自定义数据进行补充
    private String supplementExtendedData(String dataType, String dataValue) {
        if (JACGUtil.isMapEmpty(extendedDataSupplementExtMap)) {
            // 未指定用于对自定义数据进行补充的处理类
            return dataValue;
        }

        ExtendedDataSupplementInterface extendedDataSupplementExt = extendedDataSupplementExtMap.get(dataType);
        if (extendedDataSupplementExt == null) {
            // 未指定用于对当前自定义数据进行补充的处理类
            return dataValue;
        }

        return extendedDataSupplementExt.supplement(dataValue);
    }

    // 确定写入输出文件的当前调用方法信息
    private String chooseCalleeInfo(String callerFullMethod, String callerFullClassName, String callerMethodName, String callerClassName) {
        if (confInfo.getCallGraphOutputDetail().equals(JACGConstants.CONFIG_OUTPUT_DETAIL_1)) {
            // # 1: 展示 完整类名+方法名+方法参数
            return callerFullMethod;
        } else if (confInfo.getCallGraphOutputDetail().equals(JACGConstants.CONFIG_OUTPUT_DETAIL_2)) {
            // # 2: 展示 完整类名+方法名
            return callerFullClassName + JACGConstants.FLAG_COLON + callerMethodName;
        }
        // # 3: 展示 简单类名（对于同名类展示完整类名）+方法名
        return callerClassName + JACGConstants.FLAG_COLON + callerMethodName;
    }

    // 确定查询调用关系时所需字段
    private String chooseSelectMethodColumns() {
        Set<String> columnSet = new HashSet<>();
        columnSet.add(DC.MC_ID);
        columnSet.add(DC.MC_CALL_TYPE);
        columnSet.add(DC.MC_CALLEE_FULL_METHOD);
        columnSet.add(DC.MC_ENABLED);
        columnSet.add(DC.MC_CALLEE_METHOD_HASH);
        // 以下为查询手工添加的自定义数据时需要使用
        columnSet.add(DC.MC_CALLER_FULL_METHOD);

        if (confInfo.getCallGraphOutputDetail().equals(JACGConstants.CONFIG_OUTPUT_DETAIL_1)) {
            // # 1: 展示 完整类名+方法名+方法参数
        } else if (confInfo.getCallGraphOutputDetail().equals(JACGConstants.CONFIG_OUTPUT_DETAIL_2)) {
            // # 2: 展示 完整类名+方法名
        } else {
            // # 3: 展示 简单类名（对于同名类展示完整类名）+方法名
            columnSet.add(DC.MC_CALLEE_CLASS_NAME);
        }

        if (confInfo.isShowCallerLineNum()) {
            // 显示调用者代码行号
            columnSet.add(DC.MC_CALLER_CLASS_NAME);
            columnSet.add(DC.MC_CALLER_LINE_NUM);
        }

        return StringUtils.join(columnSet.toArray(), JACGConstants.FLAG_COMMA_WITH_SPACE);
    }

    // 获取调用者完整类名
    private String getCallerFullClassName(String callerClassName) {
        // 根据简单类名，查找对应的完整类名
        String sqlKey = JACGConstants.SQL_KEY_MC_QUERY_CALLER_FULL_CLASS;
        String sql = sqlCacheMap.get(sqlKey);
        if (sql == null) {
            sql = "select " + DC.MC_CALLER_FULL_CLASS_NAME + " from " + JACGConstants.TABLE_PREFIX_METHOD_CALL + confInfo.getAppName() +
                    " where " + DC.MC_CALLER_CLASS_NAME + " = ? limit 1";
            cacheSql(sqlKey, sql);
        }

        List<Object> fullClassNameList = dbOperator.queryListOneColumn(sql, new Object[]{callerClassName});
        if (JACGUtil.isCollectionEmpty(fullClassNameList)) {
            logger.warn("从方法调用关系表未找到对应的完整类名 {}", callerClassName);
            return null;
        }

        return (String) fullClassNameList.get(0);
    }

    /**
     * 当方法名为以下前缀时，忽略
     *
     * @param methodNameWithArgs 方法名，包含参数
     * @return true: 忽略，false: 需要处理
     */
    protected boolean isEntryMethodIgnoredWithPrefixByMethodNameWithArgs(String methodNameWithArgs) {
        for (String entryMethodIgnorePrefix : entryMethodIgnorePrefixSet) {
            if (methodNameWithArgs.startsWith(entryMethodIgnorePrefix)) {
                return true;
            }
        }
        return false;
    }


    /**
     * 当完整方法（类名+方法名+参数）为以下前缀时，忽略
     *
     * @param fullMethod 完整方法（类名+方法名+参数）
     * @return true: 忽略，false: 需要处理
     */
    private boolean isIgnoredFullMethodWithPrefixByFullMethod(String fullMethod) {
        for (String ignoreFullMethodPrefix : ignoreFullMethodPrefixSet) {
            if (fullMethod.startsWith(ignoreFullMethodPrefix)) {
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
    private boolean isIgnoredMethodWithPrefixByMethodName(String methodName) {
        for (String ignoreMethodPrefix : ignoreMethodPrefixSet) {
            if (methodName.startsWith(ignoreMethodPrefix)) {
                return true;
            }
        }
        return false;
    }

    /**
     * 当类名包含以下关键字时，忽略
     *
     * @param fullClassName 完整类名
     * @return true: 忽略，false: 需要处理
     */
    private boolean isIgnoredClassWithKeywordByFullClass(String fullClassName) {
        for (String ignoreClassKeyword : ignoreClassKeywordSet) {
            if (fullClassName.contains(ignoreClassKeyword)) {
                return true;
            }
        }
        return false;
    }
}
