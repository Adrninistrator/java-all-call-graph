package com.adrninistrator.jacg.runner;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.dto.CallerTaskInfo;
import com.adrninistrator.jacg.dto.TmpNode4Caller;
import com.adrninistrator.jacg.enums.CallTypeEnum;
import com.adrninistrator.jacg.extensions.extended_data_supplement.ExtendedDataSupplementInterface;
import com.adrninistrator.jacg.runner.base.AbstractRunnerGenCallGraph;
import com.adrninistrator.jacg.util.CommonUtil;
import com.adrninistrator.jacg.util.FileUtil;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.*;

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

    // 保存用于对自定义数据进行补充的处理类
    private Map<String, ExtendedDataSupplementInterface> extendedDataSupplementMap;

    static {
        runner = new RunnerGenAllGraph4Caller();
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
        if (!createOutputDit(JACGConstants.DIR_OUTPUT_GRAPH_FOR_CALLER)) {
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

        // 添加用于对自定义数据进行补充的处理类
        if (!addExtendedDataSupplementExtensions()) {
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

        // 打印提示信息
        printNoticeInfo();
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

        // 生成需要执行的任务信息
        List<CallerTaskInfo> callerTaskInfoList = genCallerTaskInfo();
        if (CommonUtil.isCollectionEmpty(callerTaskInfoList)) {
            return false;
        }

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

        return true;
    }

    // 设置生成调用链时的详细程度为最详细
    public static void setCallGraphOutputDetailMost() {
        System.setProperty(JACGConstants.KEY_CALL_GRAPH_OUTPUT_DETAIL, JACGConstants.CONFIG_OUTPUT_DETAIL_1);
    }

    // 添加用于对自定义数据进行补充的处理类
    private boolean addExtendedDataSupplementExtensions() {
        String extendedDataSupplementFilePath = JACGConstants.DIR_EXTENSIONS + File.separator + JACGConstants.FILE_EXTENSIONS_EXTENDED_DATA_SUPPLEMENT;

        Set<String> extendedDataSupplementClasses = FileUtil.readFile2Set(extendedDataSupplementFilePath);
        if (CommonUtil.isCollectionEmpty(extendedDataSupplementClasses)) {
            logger.info("未指定用于对自定义数据进行补充的类，跳过 {}", extendedDataSupplementFilePath);
            return true;
        }

        extendedDataSupplementMap = new HashMap<>(extendedDataSupplementClasses.size());

        try {
            for (String extensionClass : extendedDataSupplementClasses) {
                Class clazz = Class.forName(extensionClass);
                Object obj = clazz.newInstance();
                if (!(obj instanceof ExtendedDataSupplementInterface)) {
                    logger.error("指定的用于对自定义数据进行补充的类 {} 不是 {} 的实现类", extensionClass, ExtendedDataSupplementInterface.class.getName());
                    return false;
                }

                ExtendedDataSupplementInterface extendedDataSupplement = (ExtendedDataSupplementInterface) obj;
                extendedDataSupplement.init();

                ExtendedDataSupplementInterface existedClass = extendedDataSupplementMap.putIfAbsent(extendedDataSupplement.getDataType(), extendedDataSupplement);
                if (existedClass != null) {
                    logger.error("指定的用于对自定义数据进行补充的类，存在重复的类型 {} {} {}", extendedDataSupplement.getDataType(), extensionClass, existedClass.getClass().getName());
                    return false;
                }
            }
        } catch (Exception e) {
            logger.error("error ", e);
            return false;
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
        String sql = "select distinct(" + DC.MAED_CALLER_FULL_METHOD + "), " + DC.MAED_CALLEE_FULL_METHOD + " from " + JACGConstants.TABLE_PREFIX_MANUAL_ADD_EXTENDED_DATA + confInfo.getAppName();
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

                if (!CommonUtil.isNumStr(arrayRight[0]) || !CommonUtil.isNumStr(arrayRight[1])) {
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
            return false;
        }

        // 获取调用者完整类名
        String callerFullClassName = getCallerFullClassName(callerClassName);
        if (StringUtils.isBlank(callerFullClassName)) {
            return false;
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
        if (CommonUtil.isCollectionEmpty(callerMethodList)) {
            logger.error("从方法调用关系表未找到指定的调用方法 {} {}", callerClassName, fullMethodPrefix);
            return false;
        }

        boolean findMethod = false;
        String callerMethodHash = null;
        String callerFullMethod = null;

        // 遍历找到的方法
        for (Map<String, Object> callerMethodMap : callerMethodList) {
            String currentCallerFullMethod = (String) callerMethodMap.get(DC.MC_CALLER_FULL_METHOD);
            String currentMethodWithArgs = CommonUtil.getMethodWithArgs(currentCallerFullMethod);

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
        String callerMethodName = CommonUtil.getOnlyMethodName(callerFullMethod);

        // 确定当前方法对应输出文件名，格式: 配置文件中指定的类名（简单类名或全名）+方法名+方法名hash.txt
        StringBuilder sbOutputFileName = new StringBuilder(outputDirPrefix).append(File.separator).append(callerClassName)
                .append(JACGConstants.FLAG_AT).append(callerMethodName).append(JACGConstants.FLAG_AT).append(callerMethodHash);
        if (lineNumStart != JACGConstants.LINE_NUM_NONE && lineNumEnd != JACGConstants.LINE_NUM_NONE) {
            sbOutputFileName.append(JACGConstants.FLAG_AT).append(lineNumStart).append(JACGConstants.FLAG_MINUS).append(lineNumEnd);
        }
        sbOutputFileName.append(JACGConstants.EXT_TXT);
        String outputFileName = sbOutputFileName.toString();
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

    /**
     * 根据指定的调用者方法HASH，查找所有被调用方法信息
     *
     * @param callerMethodHash
     * @param out
     * @param callerFullMethod
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
                node4CallerList.get((currentNodeLevel)).setCurrentCalleeMethodId(currentMethodCallId);

                if (enabled != JACGConstants.ENABLED) {
                    String callType = (String) calleeMethodMap.get(DC.MC_CALL_TYPE);

                    // 记录被禁用的方法调用
                    recordDisabledMethodCall(currentMethodCallId, callType);
                }

                continue;
            }

            // 当前记录需要处理
            String currentCalleeMethodHash = (String) calleeMethodMap.get(DC.MC_CALLEE_METHOD_HASH);

            // 检查是否出现循环调用
            int back2Level = checkCycleCall(node4CallerList, currentNodeLevel, currentCalleeMethodHash);

            // 记录被调用方法信息
            if (!recordCalleeInfo(calleeMethodMap, currentNodeLevel, currentCalleeMethodHash, back2Level, out, currentMethodCallId)) {
                return false;
            }

            if (back2Level != JACGConstants.NO_CYCLE_CALL_FLAG) {
                logger.info("找到循环调用 {} [{}]", currentCalleeMethodHash, back2Level);

                // 将当前处理的层级指定到循环调用的节点
                currentNodeLevel = back2Level;
                continue;
            }

            // 更新当前处理节点的id
            node4CallerList.get((currentNodeLevel)).setCurrentCalleeMethodId(currentMethodCallId);

            // 继续下一层处理
            currentNodeLevel++;

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

        String calleeFullClassName = CommonUtil.getFullClassNameFromMethod(calleeFullMethod);

        // 根据类名特定关键字判断是否需要忽略
        if (isIgnoredClassWithKeywordByFullClass(calleeFullClassName)) {
            return true;
        }

        String calleeMethodNameWithArgs = CommonUtil.getMethodWithArgs(calleeFullMethod);

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

    // 记录被调用方法信息
    protected boolean recordCalleeInfo(Map<String, Object> calleeMethodMap, int currentNodeLevel, String currentCalleeMethodHash, int back2Level,
                                       BufferedWriter out, int callId) throws IOException {
        StringBuilder calleeInfo = new StringBuilder();
        calleeInfo.append(genOutputPrefix(currentNodeLevel + 1));

        String callerFullMethod = (String) calleeMethodMap.get(DC.MC_CALLER_FULL_METHOD);
        String calleeFullMethod = (String) calleeMethodMap.get(DC.MC_CALLEE_FULL_METHOD);

        // 显示调用者代码行号
        if (confInfo.isShowCallerLineNum()) {
            calleeInfo.append(JACGConstants.FLAG_LEFT_PARENTHESES)
                    .append((String) calleeMethodMap.get(DC.MC_CALLER_CLASS_NAME))
                    .append(JACGConstants.FLAG_COLON)
                    .append((int) calleeMethodMap.get(DC.MC_CALLER_LINE_NUM))
                    .append(JACGConstants.FLAG_RIGHT_PARENTHESES)
                    .append(JACGConstants.FLAG_TAB);
        }

        if (confInfo.getCallGraphOutputDetail().equals(JACGConstants.CONFIG_OUTPUT_DETAIL_1)) {
            // # 1: 展示 完整类名+方法名+方法参数
            calleeInfo.append(calleeFullMethod);
        } else if (confInfo.getCallGraphOutputDetail().equals(JACGConstants.CONFIG_OUTPUT_DETAIL_2)) {
            // # 2: 展示 完整类名+方法名
            String calleeFullClassName = CommonUtil.getFullClassNameFromMethod(calleeFullMethod);
            String calleeMethodName = CommonUtil.getOnlyMethodName(calleeFullMethod);

            calleeInfo.append(calleeFullClassName)
                    .append(JACGConstants.FLAG_COLON)
                    .append(calleeMethodName);
        } else {
            // # 3: 展示 简单类名（对于同名类展示完整类名）+方法名
            String calleeMethodName = CommonUtil.getOnlyMethodName(calleeFullMethod);

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

        // 添加循环调用标志
        if (back2Level != JACGConstants.NO_CYCLE_CALL_FLAG) {
            calleeInfo.append(String.format(JACGConstants.CALL_FLAG_CYCLE, back2Level));
        }

        // 添加自定义数据
        if (!addExtendedData(callId, calleeInfo, currentCalleeMethodHash, callerFullMethod, calleeFullMethod)) {
            return false;
        }

        out.write(calleeInfo.toString());
        out.write(JACGConstants.NEW_LINE);
        String callType = (String) calleeMethodMap.get(DC.MC_CALL_TYPE);

        // 记录可能出现一对多的方法调用
        return recordMethodCallMayBeMulti(callId, callType);
    }

    // 添加自定义数据
    private boolean addExtendedData(int callId, StringBuilder calleeInfo, String calleeMethodHash, String callerFullMethod, String calleeFullMethod) {
        // 处理手工添加的自定义数据
        Boolean success = handleManualAddExtendedData(callId, calleeMethodHash, callerFullMethod, calleeFullMethod, calleeInfo);
        if (success == null) {
            // 处理失败
            return false;
        }
        if (Boolean.TRUE == success) {
            // 处理了手工添加的自定义数据，返回成功
            return true;
        }

        // 不存在手工添加的自定义数据，需要继续处理程序识别的自定义数据
        if (callIdWithExtendedDataSet == null || !callIdWithExtendedDataSet.contains(callId)) {
            // 不存在程序识别的自定义数据，返回成功
            return true;
        }

        String sqlKey = JACGConstants.SQL_KEY_ED_QUERY_EXTENDED_DATA;
        String sql = sqlCacheMap.get(sqlKey);
        if (sql == null) {
            String columns = StringUtils.joinWith(JACGConstants.FLAG_COMMA_WITH_SPACE, DC.ED_DATA_TYPE, DC.ED_DATA_VALUE);
            sql = "select " + columns + " from " + JACGConstants.TABLE_PREFIX_EXTENDED_DATA + confInfo.getAppName() +
                    " where " + DC.ED_CALL_ID + " = ?";
            cacheSql(sqlKey, sql);
        }

        Map<String, Object> extendedDataMap = dbOperator.queryOneRow(sql, new Object[]{callId});
        if (CommonUtil.isMapEmpty(extendedDataMap)) {
            logger.error("查询自定义数据不存在 {}", callId);
            return false;
        }

        String dataType = (String) extendedDataMap.get(DC.ED_DATA_TYPE);
        // 对自定义数据进行补充
        String dataValueAfterSupplement = supplementExtendedData(dataType, (String) extendedDataMap.get(DC.ED_DATA_VALUE));

        calleeInfo.append(JACGConstants.CALL_FLAG_EXTENDED_DATA)
                .append(dataType)
                .append(JACGConstants.FLAG_AT)
                .append(dataValueAfterSupplement);

        return true;
    }

    /**
     * 处理手工添加的自定义数据
     *
     * @param callId
     * @param calleeMethodHash
     * @param callerFullMethod
     * @param calleeFullMethod
     * @param calleeInfo
     * @return TRUE: 处理成功, FALSE: 不存在手工添加的自定义数据, null: 处理失败
     */
    private Boolean handleManualAddExtendedData(int callId, String calleeMethodHash, String callerFullMethod, String calleeFullMethod, StringBuilder calleeInfo) {
        Set<String> calleeFullMethodSet = callFullMethodWithMAEDMap.get(callerFullMethod);
        if (calleeFullMethodSet == null || !calleeFullMethodSet.contains(calleeFullMethod)) {
            // 当前调用者及被调用者方法不存在手工添加的自定义数据
            return Boolean.FALSE;
        }

        // 查询当前调用者中，被调用者方法出现的序号
        String sqlKey = JACGConstants.SQL_KEY_MC_QUERY_CALLEE_SEQ_IN_CALLER;
        String sql4CalleeSeq = sqlCacheMap.get(sqlKey);
        if (sql4CalleeSeq == null) {
            sql4CalleeSeq = "select count(*) from " + JACGConstants.TABLE_PREFIX_METHOD_CALL + confInfo.getAppName() +
                    " where " + DC.MC_CALLEE_METHOD_HASH + " = ? and " + DC.MC_CALLER_FULL_METHOD + " = ? and " + DC.MC_ID + " <= ? order by " + DC.MC_ID;
            cacheSql(sqlKey, sql4CalleeSeq);
        }

        List<Object> list4CalleeSeq = dbOperator.queryListOneColumn(sql4CalleeSeq, new Object[]{calleeMethodHash, callerFullMethod, callId});
        if (CommonUtil.isCollectionEmpty(list4CalleeSeq)) {
            logger.error("查询当前调用者中，被调用者方法出现的序号失败 {} {} {}", calleeMethodHash, callerFullMethod, callId);
            return null;
        }

        long calleeSeqInCaller = (long) list4CalleeSeq.get(0);
        if (calleeSeqInCaller <= 0) {
            logger.error("查询当前调用者中，被调用者方法出现的序号失败2 {} {} {}", calleeMethodHash, callerFullMethod, callId);
            return null;
        }

        // 查询当前调用关系手工添加的自定义数据
        String sqlKey4MAED = JACGConstants.SQL_KEY_MAED_QUERY_MANUAL_ADD_EXTENDED_DATA;
        String sql4MAED = sqlCacheMap.get(sqlKey4MAED);
        if (sql4MAED == null) {
            String columns = StringUtils.joinWith(JACGConstants.FLAG_COMMA_WITH_SPACE, DC.MAED_DATA_TYPE, DC.MAED_DATA_VALUE);
            sql4MAED = "select " + columns + " from " + JACGConstants.TABLE_PREFIX_MANUAL_ADD_EXTENDED_DATA + confInfo.getAppName() +
                    " where " + DC.MAED_CALLER_FULL_METHOD + " = ? and " + DC.MAED_CALLEE_FULL_METHOD + " = ? and " + DC.MAED_CALLEE_SEQ_IN_CALLER + " = ?";
            cacheSql(sqlKey4MAED, sql4MAED);
        }

        List<Map<String, Object>> list4MAED = dbOperator.queryList(sql4MAED, new Object[]{callerFullMethod, calleeFullMethod, calleeSeqInCaller});
        if (list4MAED == null) {
            logger.error("查询当前调用关系手工添加的自定义数据失败 {} {} {}", callerFullMethod, calleeFullMethod, calleeSeqInCaller);
            return null;
        }
        if (list4MAED.isEmpty()) {
            // 当前调用关系不存在手工添加的自定义数据
            return Boolean.FALSE;
        }

        if (list4MAED.size() > 1) {
            logger.error("当前调用关系存在多条手工添加的自定义数据，请仅保留一条 {} {} {}", callerFullMethod, calleeFullMethod, calleeSeqInCaller);
            return null;
        }

        Map<String, Object> map4MAED = list4MAED.get(0);

        String dataType = (String) map4MAED.get(DC.MAED_DATA_TYPE);
        // 对自定义数据进行补充
        String dataValueAfterSupplement = supplementExtendedData(dataType, (String) map4MAED.get(DC.MAED_DATA_VALUE));

        calleeInfo.append(JACGConstants.CALL_FLAG_EXTENDED_DATA_MANUAL_ADD)
                .append(dataType)
                .append(JACGConstants.FLAG_AT)
                .append(dataValueAfterSupplement);

        return Boolean.TRUE;
    }

    // 对自定义数据进行补充
    private String supplementExtendedData(String dataType, String dataValue) {
        if (CommonUtil.isMapEmpty(extendedDataSupplementMap)) {
            // 未指定用于对自定义数据进行补充的处理类
            return dataValue;
        }

        ExtendedDataSupplementInterface extendedDataSupplement = extendedDataSupplementMap.get(dataType);
        if (extendedDataSupplement == null) {
            // 未指定用于对当前自定义数据进行补充的处理类
            return dataValue;
        }

        return extendedDataSupplement.supplement(dataValue);
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
        if (CommonUtil.isCollectionEmpty(fullClassNameList)) {
            logger.error("从方法调用关系表未找到对应的完整类名 {}", callerClassName);
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
