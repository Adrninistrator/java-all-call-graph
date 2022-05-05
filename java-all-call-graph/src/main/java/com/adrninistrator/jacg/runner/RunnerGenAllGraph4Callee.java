package com.adrninistrator.jacg.runner;

import com.adrninistrator.jacg.annotation.AnnotationStorage;
import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.common.enums.OutputDetailEnum;
import com.adrninistrator.jacg.dto.TmpNode4Callee;
import com.adrninistrator.jacg.dto.task.CalleeTaskInfo;
import com.adrninistrator.jacg.dto.task.CalleeTmpMethodInfo;
import com.adrninistrator.jacg.dto.task.FindMethodInfo;
import com.adrninistrator.jacg.runner.base.AbstractRunnerGenCallGraph;
import com.adrninistrator.jacg.util.FileUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.MutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.*;

/**
 * @author adrninistrator
 * @date 2021/6/20
 * @description: 从数据库读取数据，生成调用指定类的所有向上的调用关系
 */

public class RunnerGenAllGraph4Callee extends AbstractRunnerGenCallGraph {

    private static final Logger logger = LoggerFactory.getLogger(RunnerGenAllGraph4Callee.class);

    static {
        runner = new RunnerGenAllGraph4Callee();
    }

    @Override
    public boolean init() {
        // 检查Jar包文件是否有更新
        if (checkJarFileUpdated()) {
            return false;
        }

        // 读取配置文件中指定的需要处理的任务
        if (!readTaskInfo(OtherConfigFileUseSetEnum.OCFUSE_OUT_GRAPH_FOR_CALLEE_CLASS_NAME)) {
            return false;
        }

        // 创建输出文件所在目录
        if (!createOutputDir(JACGConstants.DIR_OUTPUT_GRAPH_FOR_CALLEE)) {
            return false;
        }

        String dirPath = outputDirPrefix + File.separator + JACGConstants.DIR_METHODS;
        if (!FileUtil.isDirectoryExists(dirPath)) {
            return false;
        }

        // 添加用于添加对方法上的注解进行处理的类
        if (!addMethodAnnotationHandlerExtensions()) {
            return false;
        }

        return true;
    }

    @Override
    public void operate() {
        if (!doOperate()) {
            // 记录执行失败的任务信息
            recordTaskFail();
            return;
        }

        if (someTaskFail) {
            return;
        }

        // 将输出的方法文件合并为类对应的文件
        combineClassFile();

        // 将输出文件合并
        combineOutputFile(JACGConstants.COMBINE_FILE_NAME_4_CALLEE);

        // 生成映射文件
        writeMappingFile();

        // 打印提示信息
        printNoticeInfo();
    }

    private boolean doOperate() {
        // 读取方法注解
        if (confInfo.isShowMethodAnnotation() && !AnnotationStorage.init(dbOperator, confInfo.getAppName())) {
            return false;
        }

        // 生成需要处理的任务信息
        Map<String, CalleeTaskInfo> calleeTaskInfoMap = genCalleeTaskInfo();
        if (JACGUtil.isMapEmpty(calleeTaskInfoMap)) {
            logger.error("执行失败，请检查配置文件 {} 的内容", OtherConfigFileUseSetEnum.OCFUSE_OUT_GRAPH_FOR_CALLEE_CLASS_NAME);
            return false;
        }

        // 创建线程，不指定任务数量，因为在对类进行处理时实际需要处理的方法数无法提前知道
        createThreadPoolExecutor(null);

        // 遍历需要处理的任务
        for (Map.Entry<String, CalleeTaskInfo> calleeTaskInfoEntry : calleeTaskInfoMap.entrySet()) {
            // 处理一个被调用类
            if (!handleOneCalleeClass(calleeTaskInfoEntry)) {
                // 等待直到任务执行完毕
                wait4TPEDone();
                return false;
            }
        }

        // 等待直到任务执行完毕
        wait4TPEDone();
        return true;
    }

    // 生成需要处理的任务信息
    private Map<String, CalleeTaskInfo> genCalleeTaskInfo() {
        /*
            当前方法返回的Map，每个键值对代表一个类
            含义
            key: 类名（简单类名或完整类名）
            value: 任务信息
         */
        Map<String, CalleeTaskInfo> calleeTaskInfoMap = new HashMap<>();
        // 生成需要处理的类名Set
        for (String task : taskSet) {
            String[] taskArray = task.split(JACGConstants.FLAG_COLON);
            if (taskArray.length != 1 && taskArray.length != 2) {
                logger.error("配置文件 {} 中指定的任务信息非法\n{}\n格式应为以下之一:\n" +
                                "1. [类名] （代表生成指定类所有方法向上的调用链）\n" +
                                "2. [类名]:[方法名] （代表生成指定类指定名称方法向上的调用链）\n" +
                                "3. [类名]:[方法中的代码行号] （代表生成指定类指定代码行号对应方法向上的调用链）",
                        OtherConfigFileUseSetEnum.OCFUSE_OUT_GRAPH_FOR_CALLEE_CLASS_NAME, task);
                return null;
            }

            String className = taskArray[0];

            // 获取简单类名
            String simpleClassName = getSimpleClassName(className);
            if (simpleClassName == null) {
                return null;
            }

            CalleeTaskInfo calleeTaskInfo = calleeTaskInfoMap.computeIfAbsent(simpleClassName, k -> new CalleeTaskInfo());
            if (taskArray.length == 1) {
                // 仅指定了类名，需要处理所有的方法
                if (calleeTaskInfo.getMethodInfoMap() != null) {
                    logger.warn("{} 类指定了处理指定方法，也指定了处理全部方法，对该类的全部方法都会进行处理", simpleClassName);
                }

                calleeTaskInfo.setGenAllMethods(true);
            } else {
                // 除类名外还指定了方法信息，只处理指定的方法
                if (calleeTaskInfo.isGenAllMethods()) {
                    logger.warn("{} 类指定了处理全部方法，也指定了处理指定方法，对该类的全部方法都会进行处理", simpleClassName);
                    continue;
                }

                Map<String, String> methodInfoMap = calleeTaskInfo.getMethodInfoMap();
                if (methodInfoMap == null) {
                    methodInfoMap = new HashMap<>();
                    calleeTaskInfo.setMethodInfoMap(methodInfoMap);
                }
                String methodInfo = taskArray[1];
                 /*
                    以下put的数据：
                    key: 配置文件中指定的任务原始文本
                    value: 配置文件中指定的方法名或代码行号
                  */
                methodInfoMap.put(task, methodInfo);

                if (!JACGUtil.isNumStr(methodInfo)) {
                    // 当有指定通过方法名而不是代码行号获取方法时，设置对应标志 
                    calleeTaskInfo.setFindMethodByName(true);
                }
            }
        }

        return calleeTaskInfoMap;
    }

    // 处理一个被调用类
    private boolean handleOneCalleeClass(Map.Entry<String, CalleeTaskInfo> calleeTaskInfoEntry) {
        String calleeSimpleClassName = calleeTaskInfoEntry.getKey();
        CalleeTaskInfo calleeTaskInfo = calleeTaskInfoEntry.getValue();

        // 查询被调用类的全部方法信息
        List<CalleeTmpMethodInfo> calleeTmpMethodInfoList = Collections.emptyList();

        if (calleeTaskInfo.isGenAllMethods() || calleeTaskInfo.isFindMethodByName()) {
            // 假如需要生成指定类的全部方法向上调用链，或需要根据方法名称查询方法时，需要查询被调用类的全部方法信息
            calleeTmpMethodInfoList = queryMethodsOfCalleeClass(calleeSimpleClassName);
            if (calleeTmpMethodInfoList == null) {
                return false;
            }
        }

        if (calleeTaskInfo.isGenAllMethods()) {
            // 需要生成指定类的全部方法向上调用链
            if (calleeTmpMethodInfoList.isEmpty()) {
                logger.error("以下类需要为所有方法生成向上方法调用链，但未查找到其他方法调用该类的方法 {}", calleeSimpleClassName);
                return false;
            }

            for (CalleeTmpMethodInfo calleeTmpMethodInfo : calleeTmpMethodInfoList) {
                // 处理一个被调用方法
                handleOneCalleeMethod(calleeSimpleClassName, calleeTmpMethodInfo.getMethodHash(), calleeTmpMethodInfo.getFullMethod(), null);
            }
            return true;
        }

        // 生成指定类的名称或代码行号匹配的方法向上调用链
        for (Map.Entry<String, String> methodInfoEntry : calleeTaskInfo.getMethodInfoMap().entrySet()) {
            String origTaskText = methodInfoEntry.getKey();
            String methodInfoInTask = methodInfoEntry.getValue();
            if (!JACGUtil.isNumStr(methodInfoInTask)) {
                // 通过方法名查找对应的方法并处理
                if (!handleOneCalleeMethodByName(calleeSimpleClassName, calleeTmpMethodInfoList, origTaskText, methodInfoInTask)) {
                    return false;
                }
            } else {
                // 通过代码行号查找对应的方法并处理
                if (!handleOneCalleeMethodByLineNumber(calleeSimpleClassName, origTaskText, methodInfoInTask)) {
                    return false;
                }
            }
        }

        return true;
    }

    // 查询被调用类的全部方法信息
    private List<CalleeTmpMethodInfo> queryMethodsOfCalleeClass(String calleeSimpleClassName) {
        List<CalleeTmpMethodInfo> calleeTmpMethodInfoList = new ArrayList<>();

        // 查找指定被调用类的全部方法
        String sqlKey = JACGConstants.SQL_KEY_MC_QUERY_CALLEE_ALL_METHODS;
        String sql = sqlCacheMap.get(sqlKey);
        if (sql == null) {
            sql = "select distinct(" + DC.MC_CALLEE_METHOD_HASH + ")," + DC.MC_CALLEE_FULL_METHOD + " from " +
                    JACGConstants.TABLE_PREFIX_METHOD_CALL + confInfo.getAppName() + " where " + DC.MC_CALLEE_CLASS_NAME + " = ?";
            cacheSql(sqlKey, sql);
        }

        List<Map<String, Object>> calleeMethodList = dbOperator.queryList(sql, new Object[]{calleeSimpleClassName});
        if (calleeMethodList == null) {
            return null;
        }

        if (calleeMethodList.isEmpty()) {
            logger.warn("从方法调用关系表未找到被调用类对应方法 [{}] [{}]", sql, calleeSimpleClassName);
            return calleeTmpMethodInfoList;
        }

        for (Map<String, Object> map : calleeMethodList) {
            String calleeMethodHash = (String) map.get(DC.MC_CALLEE_METHOD_HASH);
            String calleeFullMethod = (String) map.get(DC.MC_CALLEE_FULL_METHOD);

            CalleeTmpMethodInfo calleeTmpMethodInfo = new CalleeTmpMethodInfo();
            calleeTmpMethodInfo.setMethodHash(calleeMethodHash);
            calleeTmpMethodInfo.setFullMethod(calleeFullMethod);
            calleeTmpMethodInfo.setMethodNameAndArgs(JACGUtil.getMethodNameWithArgs(calleeFullMethod));
            calleeTmpMethodInfoList.add(calleeTmpMethodInfo);
        }

        return calleeTmpMethodInfoList;
    }

    // 处理一个被调用方法
    private void handleOneCalleeMethod(String calleeSimpleClassName, String calleeMethodHash, String calleeFullMethod, String origTaskText) {
        // 等待直到允许任务执行
        wait4TPEExecute();

        threadPoolExecutor.execute(() -> {
            try {
                // 执行处理一个被调用方法
                if (!doHandleOneCalleeMethod(calleeSimpleClassName, calleeMethodHash, calleeFullMethod, origTaskText)) {
                    // 记录执行失败的任务信息
                    recordTaskFail(origTaskText);
                }
            } catch (Exception e) {
                logger.error("error {} ", origTaskText, e);
                // 记录执行失败的任务信息
                recordTaskFail(origTaskText);
            }
        });
    }

    // 执行处理一个被调用方法
    private boolean doHandleOneCalleeMethod(String calleeSimpleClassName, String calleeMethodHash, String calleeFullMethod, String origTaskText) {
        String methodName = JACGUtil.getOnlyMethodName(calleeFullMethod);
        // 生成文件名格式: [完整或简单类名]@[方法名]@[方法HASH]
        String tmpOutputFilePath4Method = outputDirPrefix + File.separator + JACGConstants.DIR_METHODS + File.separator + calleeSimpleClassName +
                JACGConstants.FLAG_AT + methodName + JACGConstants.FLAG_AT + calleeMethodHash + JACGConstants.EXT_TXT;
        String outputFilePath4Method = JACGUtil.getSafeFileName(tmpOutputFilePath4Method);
        logger.info("当前方法输出文件名 {}", outputFilePath4Method);

        if (origTaskText != null) {
            // 指定的配置文件中任务原始文本非空时，记录映射关系
            methodInConfAndFileMap.put(origTaskText, outputFilePath4Method);
        }

        // 判断文件是否生成过
        if (writtenFileNameMap.putIfAbsent(outputFilePath4Method, Boolean.TRUE) != null) {
            logger.info("当前文件已生成过，不再处理 {} {} {}", origTaskText, calleeFullMethod, outputFilePath4Method);
            return true;
        }

        try (BufferedWriter out4Method = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(outputFilePath4Method), StandardCharsets.UTF_8))) {
            if (confInfo.isWriteConf()) {
                // 在结果文件中写入配置信息
                out4Method.write(confInfo.toString() + JACGConstants.NEW_LINE);
            }
            // 记录一个被调用方法的调用链信息
            return recordOneCalleeMethod(calleeSimpleClassName, calleeMethodHash, calleeFullMethod, out4Method);
        } catch (Exception e) {
            logger.error("error {} {} ", calleeSimpleClassName, outputFilePath4Method, e);
            return false;
        }
    }

    // 记录一个被调用方法的调用链信息
    private boolean recordOneCalleeMethod(String calleeClassName, String calleeMethodHash, String calleeFullMethod, BufferedWriter out4Method) throws IOException {
        StringBuilder stringBuilder = new StringBuilder();

        // 在文件第1行写入当前方法的完整信息
        stringBuilder.append(calleeFullMethod).append(JACGConstants.NEW_LINE);

        // 确定写入输出文件的当前调用方法信息
        String callerInfo = chooseCallerInfo(calleeClassName, calleeFullMethod);

        // 第2行写入当前方法的信息
        stringBuilder.append(genOutputPrefix(0)).append(callerInfo);
        // 写入方法注解
        String methodAnnotations = getMethodAnnotationInfo(calleeMethodHash);
        if (methodAnnotations != null) {
            stringBuilder.append(methodAnnotations);
        }

        stringBuilder.append(JACGConstants.NEW_LINE);

        // 记录查找到的调用方法信息List
        List<Pair<String, Boolean>> callerMethodList = new ArrayList<>(JACGConstants.BATCH_SIZE);

        // 根据指定的调用者方法HASH，查找所有被调用的方法信息
        if (!genAllGraph4Callee(calleeMethodHash, callerMethodList, calleeFullMethod)) {
            return false;
        }

        // 记录所有的调用方法
        for (Pair<String, Boolean> pair : callerMethodList) {
            stringBuilder.append(pair.getLeft());
            if (pair.getRight()) {
                // 对于入口方法，写入标志
                stringBuilder.append(JACGConstants.CALLEE_FLAG_ENTRY);
            }
            stringBuilder.append(JACGConstants.NEW_LINE);
        }

        out4Method.write(stringBuilder.toString());
        return true;
    }

    // 通过方法名查找对应的方法并处理
    private boolean handleOneCalleeMethodByName(String calleeSimpleClassName, List<CalleeTmpMethodInfo> calleeTmpMethodInfoList, String origTaskText, String methodInfoInTask) {
        String calleeMethodHash = null;
        List<String> calleeFullMethodList = new ArrayList<>();

        // 遍历从数据库中查找到的方法信息，查找当前指定的方法名称或参数匹配的方法
        for (CalleeTmpMethodInfo calleeTmpMethodInfo : calleeTmpMethodInfoList) {
            if (StringUtils.startsWith(calleeTmpMethodInfo.getMethodNameAndArgs(), methodInfoInTask)) {
                calleeMethodHash = calleeTmpMethodInfo.getMethodHash();
                calleeFullMethodList.add(calleeTmpMethodInfo.getFullMethod());
            }
        }

        if (calleeFullMethodList.isEmpty()) {
            // 未查找到匹配的方法，生成空文件
            return genEmptyFile(calleeSimpleClassName, methodInfoInTask);
        }

        if (calleeFullMethodList.size() > 1) {
            // 查找到匹配的方法多于1个，返回处理失败
            logger.error("根据配置文件 {} 中的方法前缀 {} 查找到的方法多于1个，请指定更精确的方法信息\n{}", OtherConfigFileUseSetEnum.OCFUSE_OUT_GRAPH_FOR_CALLEE_CLASS_NAME,
                    origTaskText, StringUtils.join(calleeFullMethodList, "\n"));
            return false;
        }

        // 处理一个被调用方法
        handleOneCalleeMethod(calleeSimpleClassName, calleeMethodHash, calleeFullMethodList.get(0), origTaskText);
        return true;
    }

    // 通过代码行号查找对应的方法并处理
    private boolean handleOneCalleeMethodByLineNumber(String calleeSimpleClassName, String origTaskText, String methodInfoInTask) {
        int methodLineNum = Integer.parseInt(methodInfoInTask);

        // 执行通过代码行号获取调用者方法
        FindMethodInfo findMethodInfo = doFindCallerMethodByLineNumber(calleeSimpleClassName, methodLineNum);
        if (findMethodInfo.isError()) {
            // 返回处理失败
            return false;
        } else if (findMethodInfo.isGenEmptyFile()) {
            // 需要生成空文件
            return genEmptyFile(calleeSimpleClassName, methodInfoInTask);
        }

        // 处理一个被调用方法
        handleOneCalleeMethod(calleeSimpleClassName, findMethodInfo.getMethodHash(), findMethodInfo.getFullMethod(), origTaskText);
        return true;
    }

    // 生成空文件
    private boolean genEmptyFile(String calleeSimpleClassName, String methodInfoInTask) {
        String tmpOutputFilePath4EmptyFile = outputDirPrefix + File.separator + JACGConstants.DIR_METHODS + File.separator + calleeSimpleClassName +
                JACGConstants.FLAG_AT + methodInfoInTask + JACGConstants.EXT_EMPTY_TXT;
        String outputFilePath4EmptyFile = JACGUtil.getSafeFileName(tmpOutputFilePath4EmptyFile);
        logger.info("生成空文件 {} {} {}", calleeSimpleClassName, methodInfoInTask, outputFilePath4EmptyFile);
        // 创建文件
        return FileUtil.createNewFile(outputFilePath4EmptyFile);
    }

    /**
     * 根据指定的被调用者方法HASH，查找所有调用方法信息
     *
     * @param calleeMethodHash
     * @param callerMethodList
     * @param calleeFullMethod
     * @return
     */
    protected boolean genAllGraph4Callee(String calleeMethodHash, List<Pair<String, Boolean>> callerMethodList, String calleeFullMethod) {

        // 通过List记录当前遍历到的节点信息（当作不定长数组使用）
        List<TmpNode4Callee> node4CalleeList = new ArrayList<>();

        // 初始加入最下层节点，callerMethodHash设为null
        TmpNode4Callee headNode = TmpNode4Callee.genNode(calleeMethodHash, null);
        node4CalleeList.add(headNode);

        // 记录当前处理的节点层级
        int currentNodeLevel = 0;

        int lineNum = 0;

        while (true) {
            TmpNode4Callee currentNode = node4CalleeList.get(currentNodeLevel);

            // 查询当前节点的一个上层调用方法
            Map<String, Object> methodMapByCallee = queryOneByCalleeMethod(currentNode);
            if (methodMapByCallee == null) {
                // 查询失败
                return false;
            }

            if (methodMapByCallee.isEmpty()) {
                // 未查询到记录
                if (currentNodeLevel <= 0) {
                    // 当前处理的节点为最下层节点，结束循环
                    // 将调用方法列表中最后一条记录设置为入口方法
                    markMethodAsEntry(callerMethodList);
                    return true;
                }

                // 当前处理的节点不是最下层节点，返回下一层处理
                currentNodeLevel--;

                // 将调用方法列表中最后一条记录设置为入口方法
                markMethodAsEntry(callerMethodList);
                continue;
            }

            // 查询到记录
            lineNum++;
            if (lineNum % JACGConstants.NOTICE_LINE_NUM == 0) {
                logger.info("记录数达到 {} {}", lineNum, calleeFullMethod);
            }

            String currentCallerMethodHash = (String) methodMapByCallee.get(DC.MC_CALLER_METHOD_HASH);
            int enabled = (Integer) methodMapByCallee.get(DC.MC_ENABLED);

            // 判断是否需要忽略
            if (enabled != JACGConstants.ENABLED) {
                // 当前记录需要忽略
                // 更新当前处理节点的调用者方法HASH
                node4CalleeList.get(currentNodeLevel).setCurrentCallerMethodHash(currentCallerMethodHash);

                Integer id = (Integer) methodMapByCallee.get(DC.MC_ID);
                String callType = (String) methodMapByCallee.get(DC.MC_CALL_TYPE);

                // 记录被禁用的方法调用
                recordDisabledMethodCall(id, callType);
                continue;
            }
            // 检查是否出现循环调用
            int back2Level = checkCycleCall(node4CalleeList, currentNodeLevel, currentCallerMethodHash);

            // 记录调用方法信息
            if (!recordCallerInfo(methodMapByCallee, currentNodeLevel, currentCallerMethodHash, back2Level, callerMethodList)) {
                return false;
            }

            if (back2Level != JACGConstants.NO_CYCLE_CALL_FLAG) {
                logger.info("找到循环调用 {} [{}]", currentCallerMethodHash, back2Level);

                // 将当前处理的层级指定到循环调用的节点
                currentNodeLevel = back2Level;
                continue;
            }

            // 更新当前处理节点的callerMethodHash
            node4CalleeList.get(currentNodeLevel).setCurrentCallerMethodHash(currentCallerMethodHash);

            // 继续上一层处理
            currentNodeLevel++;

            // 获取上一层节点
            if (currentNodeLevel + 1 > node4CalleeList.size()) {
                // 上一层节点不存在，则需要添加，callerMethodHash设为null
                TmpNode4Callee nextNode = TmpNode4Callee.genNode(currentCallerMethodHash, null);
                node4CalleeList.add(nextNode);
            } else {
                // 上一层节点已存在，则修改值
                TmpNode4Callee nextNode = node4CalleeList.get(currentNodeLevel);
                nextNode.setCurrentCalleeMethodHash(currentCallerMethodHash);
                nextNode.setCurrentCallerMethodHash(null);
            }
        }
    }

    /**
     * 检查是否出现循环调用
     *
     * @param node4CalleeList
     * @param currentNodeLevel        不需要遍历整个List，因为后面的元素可能当前无效
     * @param currentCallerMethodHash
     * @return -1: 未出现循环调用，非-1: 出现循环依赖，值为发生循环调用的层级
     */
    private int checkCycleCall(List<TmpNode4Callee> node4CalleeList, int currentNodeLevel, String currentCallerMethodHash) {
        /*
            应该根据当前找到的callerMethodHash，从列表中找到calleeMethodHash相同的节点，以这一层级作为被循环调用的层级

            node4CalleeList中的示例
            层级:    ee   er
            [0]:    a <- b
            [1]:    b <- c
            [2]:    c <- d
            [3]:    d <- a

            第3层级: er: a，根据ee为a，找到第0层级
         */
        for (int i = currentNodeLevel; i >= 0; i--) {
            if (currentCallerMethodHash.equals(node4CalleeList.get(i).getCurrentCalleeMethodHash())) {
                return i;
            }
        }
        return JACGConstants.NO_CYCLE_CALL_FLAG;
    }

    // 将调用方法列表中最后一条记录设置为入口方法
    private void markMethodAsEntry(List<Pair<String, Boolean>> callerMethodList) {
        if (!JACGUtil.isCollectionEmpty(callerMethodList)) {
            Pair<String, Boolean> pair = callerMethodList.get(callerMethodList.size() - 1);
            pair.setValue(Boolean.TRUE);
        }
    }

    // 查询当前节点的一个上层调用方法
    private Map<String, Object> queryOneByCalleeMethod(TmpNode4Callee node) {
        // 确定通过调用方法进行查询使用的SQL语句
        String sql = chooseQueryByCalleeMethodSql(node.getCurrentCallerMethodHash());

        List<Map<String, Object>> list;
        if (node.getCurrentCallerMethodHash() == null) {
            list = dbOperator.queryList(sql, new Object[]{node.getCurrentCalleeMethodHash()});
        } else {
            list = dbOperator.queryList(sql, new Object[]{node.getCurrentCalleeMethodHash(), node.getCurrentCallerMethodHash()});
        }

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

    // 确定通过调用方法进行查询使用的SQL语句
    protected String chooseQueryByCalleeMethodSql(String callerMethodHash) {
        if (callerMethodHash == null) {
            // 第一次查询
            String sqlKey = JACGConstants.SQL_KEY_MC_QUERY_ONE_CALLER1;
            String sql = sqlCacheMap.get(sqlKey);
            if (sql == null) {
                // 确定查询被调用关系时所需字段
                String callerColumns = chooseCallerColumns();
                sql = "select " + callerColumns + " from " + JACGConstants.TABLE_PREFIX_METHOD_CALL + confInfo.getAppName() + " where " +
                        DC.MC_CALLEE_METHOD_HASH + " = ? order by " + DC.MC_CALLER_METHOD_HASH + " limit 1";
                cacheSql(sqlKey, sql);
            }
            return sql;
        }

        // 不是第一次查询
        String sqlKey = JACGConstants.SQL_KEY_MC_QUERY_ONE_CALLER2;
        String sql = sqlCacheMap.get(sqlKey);
        if (sql == null) {
            // 确定查询被调用关系时所需字段
            String callerColumns = chooseCallerColumns();
            sql = "select " + callerColumns + " from " + JACGConstants.TABLE_PREFIX_METHOD_CALL + confInfo.getAppName() + " where " +
                    DC.MC_CALLEE_METHOD_HASH + " = ? and " + DC.MC_CALLER_METHOD_HASH + " > ? order by " +
                    DC.MC_CALLER_METHOD_HASH + " limit 1";
            cacheSql(sqlKey, sql);
        }
        return sql;
    }

    // 记录调用方法信息
    protected boolean recordCallerInfo(Map<String, Object> callerMethodMap, int currentNodeLevel, String currentCallerMethodHash, int back2Level,
                                       List<Pair<String, Boolean>> callerMethodList) {
        StringBuilder callerInfo = new StringBuilder();
        callerInfo.append(genOutputPrefix(currentNodeLevel + 1));

        if (confInfo.getCallGraphOutputDetail().equals(OutputDetailEnum.ODE_1.getDetail())) {
            // # 1: 展示 完整类名+方法名+方法参数
            String fullMethod = (String) callerMethodMap.get(DC.MC_CALLER_FULL_METHOD);
            callerInfo.append(fullMethod);
        } else if (confInfo.getCallGraphOutputDetail().equals(OutputDetailEnum.ODE_2.getDetail())) {
            // # 2: 展示 完整类名+方法名
            callerInfo.append(callerMethodMap.get(DC.MC_CALLER_FULL_CLASS_NAME))
                    .append(JACGConstants.FLAG_COLON)
                    .append(callerMethodMap.get(DC.MC_CALLER_METHOD_NAME));
        } else {
            // # 3: 展示 简单类名（对于同名类展示完整类名）+方法名
            callerInfo.append(callerMethodMap.get(DC.MC_CALLER_CLASS_NAME))
                    .append(JACGConstants.FLAG_COLON)
                    .append(callerMethodMap.get(DC.MC_CALLER_METHOD_NAME));
        }

        // 添加方法注解信息
        if (confInfo.isShowMethodAnnotation()) {
            String methodAnnotations = getMethodAnnotationInfo(currentCallerMethodHash);
            if (methodAnnotations != null) {
                callerInfo.append(methodAnnotations);
            }
        }

        // 显示调用者代码行号
        if (confInfo.isShowCallerLineNum()) {
            callerInfo.append(JACGConstants.FLAG_TAB)
                    .append(JACGConstants.FLAG_LEFT_BRACKET)
                    .append((String) callerMethodMap.get(DC.MC_CALLER_CLASS_NAME))
                    .append(JACGConstants.FLAG_COLON)
                    .append((int) callerMethodMap.get(DC.MC_CALLER_LINE_NUM))
                    .append(JACGConstants.FLAG_RIGHT_BRACKET);
        }

        // 添加循环调用标志
        if (back2Level != JACGConstants.NO_CYCLE_CALL_FLAG) {
            callerInfo.append(String.format(JACGConstants.CALL_FLAG_CYCLE, back2Level));
        }

        Pair<String, Boolean> pair = new MutablePair<>(callerInfo.toString(), Boolean.FALSE);
        callerMethodList.add(pair);

        Integer id = (Integer) callerMethodMap.get(DC.MC_ID);
        String callType = (String) callerMethodMap.get(DC.MC_CALL_TYPE);

        // 记录可能出现一对多的方法调用
        return recordMethodCallMayBeMulti(id, callType);
    }

    // 确定写入输出文件的当前被调用方法信息
    private String chooseCallerInfo(String calleeClassName, String calleeFullMethod) {
        if (confInfo.getCallGraphOutputDetail().equals(OutputDetailEnum.ODE_1.getDetail())) {
            // # 1: 展示 完整类名+方法名+方法参数
            return calleeFullMethod;
        } else if (confInfo.getCallGraphOutputDetail().equals(OutputDetailEnum.ODE_2.getDetail())) {
            // # 2: 展示 完整类名+方法名
            String calleeFullClassName = JACGUtil.getFullClassNameFromMethod(calleeFullMethod);
            String calleeMethodName = JACGUtil.getOnlyMethodName(calleeFullMethod);
            return calleeFullClassName + JACGConstants.FLAG_COLON + calleeMethodName;
        }
        // # 3: 展示 简单类名（对于同名类展示完整类名）+方法名
        String calleeMethodName = JACGUtil.getOnlyMethodName(calleeFullMethod);
        return calleeClassName + JACGConstants.FLAG_COLON + calleeMethodName;
    }

    // 确定查询被调用关系时所需字段
    private String chooseCallerColumns() {
        Set<String> columnSet = new HashSet<>();
        columnSet.add(DC.MC_ID);
        columnSet.add(DC.MC_CALL_TYPE);
        columnSet.add(DC.MC_ENABLED);
        columnSet.add(DC.MC_CALLER_METHOD_HASH);

        if (confInfo.getCallGraphOutputDetail().equals(OutputDetailEnum.ODE_1.getDetail())) {
            // # 1: 展示 完整类名+方法名+方法参数
            columnSet.add(DC.MC_CALLER_FULL_METHOD);
        } else if (confInfo.getCallGraphOutputDetail().equals(OutputDetailEnum.ODE_2.getDetail())) {
            // # 2: 展示 完整类名+方法名
            columnSet.add(DC.MC_CALLER_FULL_CLASS_NAME);
            columnSet.add(DC.MC_CALLER_METHOD_NAME);
        } else {
            // # 3: 展示 简单类名（对于同名类展示完整类名）+方法名
            columnSet.add(DC.MC_CALLER_CLASS_NAME);
            columnSet.add(DC.MC_CALLER_METHOD_NAME);
        }

        if (confInfo.isShowCallerLineNum()) {
            // 显示调用者代码行号
            columnSet.add(DC.MC_CALLER_CLASS_NAME);
            columnSet.add(DC.MC_CALLER_LINE_NUM);
        }

        return StringUtils.join(columnSet.toArray(), JACGConstants.FLAG_COMMA_WITH_SPACE);
    }

    // 打印存在一对多的方法调用，自定义处理
    @Override
    protected void printMultiMethodCallCustom(String callerMethodHash, StringBuilder stringBuilder) {
        String sqlKey = JACGConstants.SQL_KEY_MC_QUERY_ALL_CALLER;
        String sql = sqlCacheMap.get(sqlKey);
        if (sql == null) {
            sql = "select distinct(" + DC.MC_CALLER_FULL_METHOD + ") from " + JACGConstants.TABLE_PREFIX_METHOD_CALL + confInfo.getAppName() +
                    " where " + DC.MC_CALLEE_METHOD_HASH + " = ? order by " + DC.MC_CALLER_FULL_METHOD;
            cacheSql(sqlKey, sql);
        }

        List<Object> list = dbOperator.queryListOneColumn(sql, new Object[]{callerMethodHash});
        if (list == null) {
            logger.error("查询所有的调用方法失败 {}", callerMethodHash);
            return;
        }

        if (list.size() <= 1) {
            return;
        }

        stringBuilder.append(JACGConstants.NEW_LINE).append(JACGConstants.NEW_LINE)
                .append("- ").append(DC.MC_CALLEE_METHOD_HASH).append(JACGConstants.NEW_LINE).append(JACGConstants.NEW_LINE)
                .append(callerMethodHash).append(JACGConstants.NEW_LINE).append(JACGConstants.NEW_LINE)
                .append("- ").append(DC.MC_CALLER_FULL_METHOD).append("（调用方法）").append(JACGConstants.NEW_LINE).append(JACGConstants.NEW_LINE)
                .append("```").append(JACGConstants.NEW_LINE);
        for (Object callerMethod : list) {
            stringBuilder.append(callerMethod).append(JACGConstants.NEW_LINE);
        }
        stringBuilder.append("```");
    }

    // 将输出的方法文件合并为类对应的文件
    private void combineClassFile() {
        List<File> methodOutputFileList = FileUtil.findFileInCurrentDir(outputDirPrefix + File.separator + JACGConstants.DIR_METHODS, JACGConstants.EXT_TXT);
        if (JACGUtil.isCollectionEmpty(methodOutputFileList)) {
            return;
        }

        String lastClassName = null;
        List<File> combineMethodFileList = new ArrayList<>();
        for (File methodOutputFile : methodOutputFileList) {
            String methodOutputFileName = methodOutputFile.getName();
            if (methodOutputFileName.endsWith(JACGConstants.EXT_EMPTY_TXT)) {
                // 跳过空文件
                continue;
            }

            String className = methodOutputFileName.split(JACGConstants.FLAG_AT)[0];
            if (lastClassName != null && !className.equals(lastClassName)) {
                // 处理到下一个类的文件，合并之前类的文件
                doCombineClassFile(lastClassName, combineMethodFileList);
                combineMethodFileList.clear();
            }

            combineMethodFileList.add(methodOutputFile);
            lastClassName = className;
        }

        if (!combineMethodFileList.isEmpty()) {
            // 合并最后一个类的文件
            doCombineClassFile(lastClassName, combineMethodFileList);
        }
    }

    // 执行将输出的方法文件合并为类对应的文件
    private void doCombineClassFile(String lastClassName, List<File> combineMethodFileList) {
        String classFilePath = outputDirPrefix + File.separator + lastClassName + JACGConstants.EXT_TXT;
        logger.info("将以下类对应的方法文件合并为类对应的文件 {}", classFilePath);
        FileUtil.combineTextFile(classFilePath, combineMethodFileList);
    }
}
