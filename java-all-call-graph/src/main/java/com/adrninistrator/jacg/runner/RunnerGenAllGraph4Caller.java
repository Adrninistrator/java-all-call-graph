package com.adrninistrator.jacg.runner;

import com.adrninistrator.jacg.common.Constants;
import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.dto.TmpNode4Caller;
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

    static {
        runner = new RunnerGenAllGraph4Caller();
    }

    @Override
    public boolean init() {
        String taskInfoFile = Constants.DIR_CONFIG + File.separator + Constants.FILE_OUT_GRAPH_FOR_CALLER_ENTRY_METHOD;
        // 读取配置文件中指定的需要处理的任务
        if (!readTaskInfo(taskInfoFile)) {
            return false;
        }

        String entryMethodIgnorePrefixFile = Constants.DIR_CONFIG + File.separator + Constants.FILE_OUT_GRAPH_FOR_CALLER_ENTRY_METHOD_IGNORE_PREFIX;
        entryMethodIgnorePrefixSet = FileUtil.readFile2Set(entryMethodIgnorePrefixFile);
        if (entryMethodIgnorePrefixSet == null) {
            return false;
        }

        // 创建输出文件所在目录
        if (!createOutputDit(Constants.DIR_OUTPUT_GRAPH_FOR_CALLER)) {
            return false;
        }

        ignoreClassKeywordSet =
                FileUtil.readFile2Set(Constants.DIR_CONFIG + File.separator + Constants.FILE_OUT_GRAPH_FOR_CALLER_IGNORE_CLASS_KEYWORD);
        if (ignoreClassKeywordSet == null) {
            return false;
        }

        ignoreFullMethodPrefixSet =
                FileUtil.readFile2Set(Constants.DIR_CONFIG + File.separator + Constants.FILE_OUT_GRAPH_FOR_CALLER_IGNORE_FULL_METHOD_PREFIX);
        if (ignoreFullMethodPrefixSet == null) {
            return false;
        }

        ignoreMethodPrefixSet =
                FileUtil.readFile2Set(Constants.DIR_CONFIG + File.separator + Constants.FILE_OUT_GRAPH_FOR_CALLER_IGNORE_METHOD_PREFIX);
        if (ignoreMethodPrefixSet == null) {
            return false;
        }
        return true;
    }

    @Override
    public void operate() {
        logger.info("{}支持忽略指定的方法", isSupportIgnore() ? "" : "不");

        // 读取方法注解
        if (confInfo.isShowMethodAnnotation() && !readMethodAnnotation()) {
            return;
        }

        // 创建线程
        createThreadPoolExecutor();

        // 遍历需要处理的任务
        for (String task : taskSet) {

            String[] array = task.split(Constants.FLAG_COLON);
            if (array == null || array.length != 2) {
                logger.error("指定的类名+方法名非法，格式应为 [类名]:[方法名] {}", task);
                return;
            }

            String callerClassName = array[0];
            String callerMethodNameInTask = array[1];

            if (StringUtils.isAnyBlank(callerClassName, callerMethodNameInTask)) {
                logger.error("指定的类名+方法名存在空值，格式应为 [类名]:[方法名] {}", task);
                return;
            }

            // 等待直到允许任务执行
            wait4TPEExecute();

            threadPoolExecutor.execute(() -> {
                // 处理一条记录
                if (!handleOneRecord(callerClassName, callerMethodNameInTask)) {
                    someTaskFail = true;
                }
            });
        }

        // 等待直到任务执行完毕
        wait4TPEDone();

        // 将输出文件合并
        combineOutputFile(Constants.COMBINE_FILE_NAME_4_CALLER);
    }

    public boolean isSupportIgnore() {
        return supportIgnore;
    }

    public void setSupportIgnore(boolean supportIgnore) {
        this.supportIgnore = supportIgnore;
    }

    // 处理一条记录
    private boolean handleOneRecord(String callerClassName, String callerMethodNameInTask) {
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
        String sql = sqlCacheMap.get(Constants.SQL_KEY_MC_QUERY_TOP_METHOD);
        if (sql == null) {
            sql = "select distinct(" + DC.MC_CALLER_METHOD_HASH + ")," + DC.MC_CALLER_FULL_METHOD + " from " +
                    Constants.TABLE_PREFIX_METHOD_CALL + confInfo.getAppName() + " where " + DC.MC_CALLER_CLASS_NAME +
                    "= ? and " + DC.MC_CALLER_FULL_METHOD + " like concat(?, '%')";
            cacheSql(Constants.SQL_KEY_MC_QUERY_TOP_METHOD, sql);
        }

        String fullMethodPrefix = callerFullClassName + Constants.FLAG_COLON + callerMethodNameInTask;

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
                        Constants.FILE_OUT_GRAPH_FOR_CALLER_ENTRY_METHOD_IGNORE_PREFIX);
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

        // 确定当前方法对应输出文件名，格式：配置文件中指定的类名（简单类名或全名）+方法名+方法名hash.txt
        String outputFileName = outputDirPrefix + File.separator + callerClassName + Constants.FLAG_AT + callerMethodName +
                Constants.FLAG_AT + callerMethodHash + Constants.EXT_TXT;
        logger.info("当前输出文件名 {}", outputFileName);

        try (BufferedWriter out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(outputFileName), StandardCharsets.UTF_8))) {
            if (confInfo.isWriteConf()) {
                // 在结果文件中写入配置信息
                out.write(confInfo.toString());
                out.write(Constants.NEW_LINE);
                out.write("supportIgnore: " + isSupportIgnore());
                out.write(Constants.NEW_LINE);
            }

            // 在文件第1行写入当前方法的完整信息
            out.write(callerFullMethod);
            out.write(Constants.NEW_LINE);

            // 确定写入输出文件的当前被调用方法信息
            String calleeInfo = chooseCalleeInfo(callerFullMethod, callerFullClassName, callerMethodName, callerClassName);

            // 第2行写入当前方法的信息
            out.write(genOutputPrefix(0));
            out.write(calleeInfo);
            // 写入方法注解
            String methodAnnotation = methodAnnotationsMap.get(callerMethodHash);
            if (methodAnnotation != null) {
                out.write(methodAnnotation);
            }

            out.write(Constants.NEW_LINE);

            // 根据指定的调用者方法HASH，查找所有被调用的方法信息
            return genAllGraph4Caller(callerMethodHash, out, callerFullMethod);
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
     * @return
     */
    protected boolean genAllGraph4Caller(String callerMethodHash, BufferedWriter out, String callerFullMethod) throws IOException {

        // 通过List记录当前遍历到的节点信息（当作不定长数组使用）
        List<TmpNode4Caller> node4CallerList = new ArrayList<>();

        // 初始加入最上层节点，id设为0（方法调用关系表最小id为1）
        TmpNode4Caller headNode = TmpNode4Caller.genNode(callerMethodHash, Constants.METHOD_CALL_ID_START);
        node4CallerList.add(headNode);

        // 记录当前处理的节点层级
        int currentNodeLevel = 0;

        int lineNum = 0;

        while (true) {
            TmpNode4Caller currentNode = node4CallerList.get(currentNodeLevel);

            // 查询当前节点的一个下层被调用方法
            Map<String, Object> methodMapByCaller = queryOneByCallerMethod(currentNode);
            if (methodMapByCaller == null) {
                // 查询失败
                return false;
            }

            if (methodMapByCaller.isEmpty()) {
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
            lineNum++;
            if (lineNum % Constants.NOTICE_LINE_NUM == 0) {
                logger.info("记录数达到 {} {}", lineNum, callerFullMethod);
            }

            int currentMethodCallId = (Integer) methodMapByCaller.get(DC.MC_ID);

            // 判断是否需要忽略
            if (isSupportIgnore() && ignoreCurrentMethod(methodMapByCaller)) {
                // 当前记录需要忽略
                // 更新当前处理节点的id
                node4CallerList.get((currentNodeLevel)).setCurrentCalleeMethodId(currentMethodCallId);
                continue;
            }

            // 当前记录需要处理
            String currentCalleeMethodHash = (String) methodMapByCaller.get(DC.MC_CALLEE_METHOD_HASH);

            // 检查是否出现循环调用
            int back2Level = checkCycleCall(node4CallerList, currentNodeLevel, currentCalleeMethodHash);

            // 记录被调用方法信息
            recordCalleeInfo(methodMapByCaller, currentNodeLevel, currentCalleeMethodHash, back2Level, out);

            if (back2Level != Constants.NO_CYCLE_CALL_FLAG) {
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
                TmpNode4Caller nextNode = TmpNode4Caller.genNode(currentCalleeMethodHash, Constants.METHOD_CALL_ID_START);
                node4CallerList.add(nextNode);
            } else {
                // 下一层节点已存在，则修改值
                TmpNode4Caller nextNode = node4CallerList.get(currentNodeLevel);
                nextNode.setCurrentCalleeMethodHash(currentCalleeMethodHash);
                nextNode.setCurrentCalleeMethodId(Constants.METHOD_CALL_ID_START);
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
        return Constants.NO_CYCLE_CALL_FLAG;
    }

    // 查询当前节点的一个下层被调用方法
    private Map<String, Object> queryOneByCallerMethod(TmpNode4Caller node) {
        // 确定通过被调用方法进行查询使用的SQL语句
        String sql = chooseQueryByCallerMethodSql();

        List<Map<String, Object>> list = dbOperator.queryList(sql, new Object[]{node.getCurrentCalleeMethodHash(), node.getCurrentCalleeMethodId()});
        if (list == null) {
            // 查询失败
            return null;
        }

        if (CommonUtil.isCollectionEmpty(list)) {
            // 查询不到结果时，返回空Map
            return new HashMap<>(0);
        }

        return list.get(0);
    }

    // 确定通过被调用方法进行查询使用的SQL语句
    protected String chooseQueryByCallerMethodSql() {
        String sql = sqlCacheMap.get(Constants.SQL_KEY_MC_QUERY_ONE_CALLEE);
        if (sql == null) {
            // 确定查询被调用关系时所需字段
            String selectMethodColumns = chooseSelectMethodColumns();

            sql = "select " + selectMethodColumns + ", " + DC.MC_CALLEE_METHOD_HASH + ", " + DC.MC_ID + " from " +
                    Constants.TABLE_PREFIX_METHOD_CALL + confInfo.getAppName() + " where " +
                    DC.MC_CALLER_METHOD_HASH + " = ? and " + DC.MC_ID + " > ? order by " + DC.MC_ID + " limit 1";
            cacheSql(Constants.SQL_KEY_MC_QUERY_ONE_CALLEE, sql);
        }
        return sql;
    }

    // 判断当前找到的被调用方法是否需要处理
    private boolean ignoreCurrentMethod(Map<String, Object> methodMapByCaller) {
        String callType = (String) methodMapByCaller.get(DC.MC_CALL_TYPE);
        String callerFullMethod = (String) methodMapByCaller.get(DC.MC_CALLER_FULL_METHOD);
        String calleeFullMethod = (String) methodMapByCaller.get(DC.MC_CALLEE_FULL_METHOD);

        // 当完整方法（类名+方法名+参数）为以下前缀时，忽略
        if (isIgnoredFullMethodWithPrefixByFullMethod(callerFullMethod) || isIgnoredFullMethodWithPrefixByFullMethod(calleeFullMethod)) {
            return true;
        }

        String callerFullClassName = CommonUtil.getFullClassNameFromMethod(callerFullMethod);
        String calleeFullClassName = CommonUtil.getFullClassNameFromMethod(calleeFullMethod);

        // 根据类名特定关键字判断是否需要忽略
        if (isIgnoredClassWithKeywordByFullClass(callerFullClassName) || isIgnoredClassWithKeywordByFullClass(calleeFullClassName)) {
            return true;
        }

        String callerMethodNameWithArgs = CommonUtil.getMethodWithArgs(callerFullMethod);
        String calleeMethodNameWithArgs = CommonUtil.getMethodWithArgs(calleeFullMethod);

        /*
            根据方法名前缀判断是否需要忽略，使用包含参数的方法名进行比较
            若当前调用类型为Runnable/Callable实现类子类构造函数调用run()方法，则不判断方法名前缀是否需要忽略（<init> -> run()，可能会被指定为忽略）
         */
        if (!StringUtils.equalsAny(callType, Constants.CALL_TYPE_RUNNABLE_INIT_RUN, Constants.CALL_TYPE_CALLABLE_INIT_CALL)
                && (isIgnoredMethodWithPrefixByMethodName(callerMethodNameWithArgs) ||
                isIgnoredMethodWithPrefixByMethodName(calleeMethodNameWithArgs))) {
            return true;
        }

        return false;
    }

    // 记录被调用方法信息
    protected void recordCalleeInfo(Map<String, Object> calleeMethodMap, int currentNodeLevel, String currentCalleeMethodHash, int back2Level,
                                    BufferedWriter out) throws IOException {
        StringBuilder calleeInfo = new StringBuilder();
        calleeInfo.append(genOutputPrefix(currentNodeLevel + 1));

        // 显示调用者代码行号
        if (confInfo.isShowCallerLineNum()) {
            calleeInfo.append(Constants.FLAG_LEFT_PARENTHESES)
                    .append((String) calleeMethodMap.get(DC.MC_CALLER_CLASS_NAME))
                    .append(Constants.FLAG_COLON)
                    .append((int) calleeMethodMap.get(DC.MC_CALLER_LINE_NUM))
                    .append(Constants.FLAG_RIGHT_PARENTHESES)
                    .append(Constants.FLAG_TAB);
        }

        if (confInfo.getCallGraphOutputDetail().equals(Constants.CONFIG_OUTPUT_DETAIL_1)) {
            // # 1: 展示 完整类名+方法名+方法参数
            calleeInfo.append(calleeMethodMap.get(DC.MC_CALLEE_FULL_METHOD));
        } else if (confInfo.getCallGraphOutputDetail().equals(Constants.CONFIG_OUTPUT_DETAIL_2)) {
            // # 2: 展示 完整类名+方法名
            String calleeFullMethod = (String) calleeMethodMap.get(DC.MC_CALLEE_FULL_METHOD);
            String calleeFullClassName = CommonUtil.getFullClassNameFromMethod(calleeFullMethod);
            String calleeMethodName = CommonUtil.getOnlyMethodName(calleeFullMethod);

            calleeInfo.append(calleeFullClassName)
                    .append(Constants.FLAG_COLON)
                    .append(calleeMethodName);
        } else {
            // # 3: 展示 简单类名（对于同名类展示完整类名）+方法名
            String calleeFullMethod = (String) calleeMethodMap.get(DC.MC_CALLEE_FULL_METHOD);
            String calleeMethodName = CommonUtil.getOnlyMethodName(calleeFullMethod);

            calleeInfo.append(calleeMethodMap.get(DC.MC_CALLEE_CLASS_NAME))
                    .append(Constants.FLAG_COLON)
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
        if (back2Level != Constants.NO_CYCLE_CALL_FLAG) {
            calleeInfo.append(String.format(Constants.CALL_FLAG_CYCLE, back2Level));
        }

        out.write(calleeInfo.toString());
        out.write(Constants.NEW_LINE);
    }

    // 确定写入输出文件的当前调用方法信息
    private String chooseCalleeInfo(String callerFullMethod, String callerFullClassName, String callerMethodName, String callerClassName) {
        if (confInfo.getCallGraphOutputDetail().equals(Constants.CONFIG_OUTPUT_DETAIL_1)) {
            // # 1: 展示 完整类名+方法名+方法参数
            return callerFullMethod;
        } else if (confInfo.getCallGraphOutputDetail().equals(Constants.CONFIG_OUTPUT_DETAIL_2)) {
            // # 2: 展示 完整类名+方法名
            return callerFullClassName + Constants.FLAG_COLON + callerMethodName;
        }
        // # 3: 展示 简单类名（对于同名类展示完整类名）+方法名
        return callerClassName + Constants.FLAG_COLON + callerMethodName;
    }

    // 确定查询调用关系时所需字段
    private String chooseSelectMethodColumns() {
        Set<String> columnSet = new HashSet<>();
        columnSet.add(DC.MC_CALL_TYPE);
        columnSet.add(DC.MC_CALLEE_FULL_METHOD);
        columnSet.add(DC.MC_CALLER_FULL_METHOD);

        if (confInfo.getCallGraphOutputDetail().equals(Constants.CONFIG_OUTPUT_DETAIL_1)) {
            // # 1: 展示 完整类名+方法名+方法参数
        } else if (confInfo.getCallGraphOutputDetail().equals(Constants.CONFIG_OUTPUT_DETAIL_2)) {
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

        return StringUtils.join(columnSet.toArray(), Constants.FLAG_COMMA_WITH_SPACE);
    }

    // 获取调用者完整类名
    private String getCallerFullClassName(String callerClassName) {
        // 根据简单类名，查找对应的完整类名
        String sql = sqlCacheMap.get(Constants.SQL_KEY_MC_QUERY_CALLER_FULL_CLASS);
        if (sql == null) {
            sql = "select " + DC.MC_CALLER_FULL_CLASS_NAME + " from " + Constants.TABLE_PREFIX_METHOD_CALL + confInfo.getAppName() +
                    " where " + DC.MC_CALLER_CLASS_NAME + " = ? limit 1";
            cacheSql(Constants.SQL_KEY_MC_QUERY_CALLER_FULL_CLASS, sql);
        }

        List<Object> fullClassNameList = dbOperator.queryListOneObject(sql, new Object[]{callerClassName});
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
     * @return true：忽略，false：需要处理
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
     * @return true：忽略，false：需要处理
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
     * @return true：忽略，false：需要处理
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
     * @return true：忽略，false：需要处理
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
