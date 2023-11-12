package com.adrninistrator.jacg.runner;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.DefaultBusinessDataTypeEnum;
import com.adrninistrator.jacg.common.enums.MethodCallFlagsEnum;
import com.adrninistrator.jacg.common.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.common.enums.OutputDetailEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.dto.annotation.BaseAnnotationAttribute;
import com.adrninistrator.jacg.dto.call_graph.CallGraphNode4Callee;
import com.adrninistrator.jacg.dto.call_graph.SuperCallChildInfo;
import com.adrninistrator.jacg.dto.method.MethodAndHash;
import com.adrninistrator.jacg.dto.task.CalleeEntryMethodTaskInfo;
import com.adrninistrator.jacg.dto.task.CalleeTaskInfo;
import com.adrninistrator.jacg.dto.task.FindMethodTaskElement;
import com.adrninistrator.jacg.dto.task.FindMethodTaskInfo;
import com.adrninistrator.jacg.dto.write_db.WriteDbData4MethodCall;
import com.adrninistrator.jacg.markdown.writer.MarkdownWriter;
import com.adrninistrator.jacg.runner.base.AbstractRunnerGenCallGraph;
import com.adrninistrator.jacg.util.JACGCallGraphFileUtil;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGFileUtil;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg.common.JavaCGCommonNameConstants;
import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.common.enums.JavaCGCallTypeEnum;
import com.adrninistrator.javacg.common.enums.JavaCGYesNoEnum;
import com.adrninistrator.javacg.dto.stack.ListAsStack;
import com.adrninistrator.javacg.util.JavaCGFileUtil;
import com.adrninistrator.javacg.util.JavaCGMethodUtil;
import com.adrninistrator.javacg.util.JavaCGUtil;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.MutablePair;
import org.apache.commons.lang3.tuple.Pair;
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

/**
 * @author adrninistrator
 * @date 2021/6/20
 * @description: 生成调用指定类方法向上的完整调用链
 */

public class RunnerGenAllGraph4Callee extends AbstractRunnerGenCallGraph {
    private static final Logger logger = LoggerFactory.getLogger(RunnerGenAllGraph4Callee.class);

    @Override
    public boolean preHandle() {
        // 公共预处理
        if (!commonPreHandle()) {
            return false;
        }

        // 读取配置文件中指定的需要处理的任务
        if (!readTaskInfo(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLEE)) {
            return false;
        }

        // 创建输出文件所在目录
        if (!createOutputDir(JACGConstants.DIR_OUTPUT_GRAPH_FOR_CALLEE)) {
            return false;
        }

        String methodsDirPath = currentOutputDirPath + File.separator + JACGConstants.DIR_OUTPUT_METHODS;
        if (!JACGFileUtil.isDirectoryExists(methodsDirPath)) {
            return false;
        }

        return true;
    }

    @Override
    public void handle() {
        // 执行实际处理
        if (!operate()) {
            // 记录执行失败的任务信息
            recordTaskFail();
            return;
        }

        if (someTaskFail) {
            return;
        }

        // 将输出的方法文件合并为类对应的文件
        combineClassFile();

        // 打印提示信息
        printNoticeInfo();
    }

    // 执行实际处理
    private boolean operate() {
        // 生成需要处理的任务信息
        Map<String, CalleeTaskInfo> calleeTaskInfoMap = genCalleeTaskInfo();
        if (JACGUtil.isMapEmpty(calleeTaskInfoMap)) {
            logger.error("执行失败，请检查配置文件 {} 的内容", OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLEE);
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
            String[] taskArray = StringUtils.splitPreserveAllTokens(task, JavaCGConstants.FLAG_COLON);
            if (taskArray.length != 1 && taskArray.length != 2) {
                logger.error("配置文件 {} 中指定的任务信息非法\n{}\n格式应为以下之一:\n" +
                                "1. [类名] （代表生成指定类所有方法向上的调用链）\n" +
                                "2. [类名]:[方法名] （代表生成指定类指定名称方法向上的调用链）\n" +
                                "3. [类名]:[方法中的代码行号] （代表生成指定类指定代码行号对应方法向上的调用链）",
                        OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLEE, task);
                return Collections.emptyMap();
            }

            String className = taskArray[0];

            // 获取唯一类名（简单类名或完整类名）
            String simpleClassName = getSimpleClassName(className);
            if (simpleClassName == null) {
                return Collections.emptyMap();
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

                if (!JavaCGUtil.isNumStr(methodInfo)) {
                    // 当有指定通过方法名而不是代码行号获取方法时，设置对应标志 
                    calleeTaskInfo.setFindMethodByName(true);
                }
            }
        }

        return calleeTaskInfoMap;
    }

    // 处理一个被调用类
    private boolean handleOneCalleeClass(Map.Entry<String, CalleeTaskInfo> calleeTaskInfoEntry) {
        String entryCalleeSimpleClassName = calleeTaskInfoEntry.getKey();
        CalleeTaskInfo calleeTaskInfo = calleeTaskInfoEntry.getValue();

        // 查询被调用类的全部方法信息
        List<FindMethodTaskElement> findMethodTaskElementList = Collections.emptyList();

        if (calleeTaskInfo.isGenAllMethods() || calleeTaskInfo.isFindMethodByName()) {
            // 假如需要生成指定类的全部方法向上调用链，或需要根据方法名称查询方法时，需要查询被调用类的全部方法信息
            findMethodTaskElementList = queryMethodsOfCalleeClass(entryCalleeSimpleClassName);
        }

        if (calleeTaskInfo.isGenAllMethods()) {
            // 需要生成指定类的全部方法向上调用链
            if (findMethodTaskElementList.isEmpty()) {
                logger.error("以下类需要为所有方法生成向上方法调用链，但未查找到其他方法调用该类的方法\n{}", entryCalleeSimpleClassName);
                return false;
            }

            for (FindMethodTaskElement findMethodTaskElement : findMethodTaskElementList) {
                // 处理一个被调用方法
                handleOneCalleeMethod(entryCalleeSimpleClassName, findMethodTaskElement, null);
            }
            return true;
        }

        // 生成指定类的名称或代码行号匹配的方法向上调用链
        for (Map.Entry<String, String> methodInfoEntry : calleeTaskInfo.getMethodInfoMap().entrySet()) {
            String origTaskText = methodInfoEntry.getKey();
            String methodInfoInTask = methodInfoEntry.getValue();
            if (!JavaCGUtil.isNumStr(methodInfoInTask)) {
                // 通过方法名查找对应的方法并处理
                if (!handleOneCalleeMethodByName(entryCalleeSimpleClassName, findMethodTaskElementList, origTaskText, methodInfoInTask)) {
                    return false;
                }
            } else {
                // 通过代码行号查找对应的方法并处理
                if (!handleOneCalleeMethodByLineNumber(entryCalleeSimpleClassName, origTaskText, methodInfoInTask)) {
                    return false;
                }
            }
        }
        return true;
    }

    // 查询被调用类的全部方法信息
    private List<FindMethodTaskElement> queryMethodsOfCalleeClass(String calleeSimpleClassName) {
        List<FindMethodTaskElement> findMethodTaskElementList = new ArrayList<>();

        // 查找指定被调用类的全部方法
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MC_QUERY_CALLEE_ALL_METHODS;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select distinct " + JACGSqlUtil.joinColumns(DC.MC_CALLEE_METHOD_HASH, DC.MC_CALLEE_FULL_METHOD, DC.MC_CALL_FLAGS, DC.MC_RAW_RETURN_TYPE) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName() +
                    " where " + DC.MC_CALLEE_SIMPLE_CLASS_NAME + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        List<WriteDbData4MethodCall> calleeMethodList = dbOperator.queryList(sql, WriteDbData4MethodCall.class, calleeSimpleClassName);
        if (JavaCGUtil.isCollectionEmpty(calleeMethodList)) {
            logger.warn("从方法调用关系表未找到被调用类对应方法 [{}] [{}]", sql, calleeSimpleClassName);
            return Collections.emptyList();
        }

        Set<String> handledCalleeMethodHashSet = new HashSet<>();
        for (WriteDbData4MethodCall methodCall : calleeMethodList) {
            if (handledCalleeMethodHashSet.add(methodCall.getCalleeMethodHash())) {
                // 根据已被处理过的方法HASH+长度判断是否需要处理，因为以上查询时返回字段增加了call_flags，因此相同的方法可能会出现多条
                FindMethodTaskElement findMethodTaskElement = new FindMethodTaskElement(methodCall.getCalleeMethodHash(), methodCall.getCalleeFullMethod(),
                        methodCall.getCallFlags(), methodCall.getRawReturnType());
                findMethodTaskElementList.add(findMethodTaskElement);
            }
        }
        return findMethodTaskElementList;
    }

    // 处理一个被调用方法
    private void handleOneCalleeMethod(String entryCalleeSimpleClassName,
                                       FindMethodTaskElement findMethodTaskElement,
                                       String origTaskText) {
        // 等待直到允许任务执行
        JACGUtil.wait4TPEExecute(threadPoolExecutor, taskQueueMaxSize);

        threadPoolExecutor.execute(() -> {
            try {
                // 执行处理一个被调用方法
                if (!doHandleOneCalleeMethod(entryCalleeSimpleClassName, findMethodTaskElement, origTaskText)) {
                    // 记录执行失败的任务信息
                    recordTaskFail(origTaskText != null ? origTaskText : findMethodTaskElement.getFullMethod());
                }
            } catch (Exception e) {
                logger.error("error {} ", origTaskText, e);
                // 记录执行失败的任务信息
                recordTaskFail(origTaskText != null ? origTaskText : findMethodTaskElement.getFullMethod());
            }
        });
    }

    // 执行处理一个被调用方法
    private boolean doHandleOneCalleeMethod(String entryCalleeSimpleClassName,
                                            FindMethodTaskElement findMethodTaskElement,
                                            String origTaskText)  {
        String entryCalleeMethodHash = findMethodTaskElement.getMethodHash();
        String entryCalleeFullMethod = findMethodTaskElement.getFullMethod();
        String entryMethodName = JACGClassMethodUtil.getMethodNameFromFull(entryCalleeFullMethod);
        // 生成方法对应的调用链文件名
        String outputFilePath4Method = currentOutputDirPath + File.separator + JACGConstants.DIR_OUTPUT_METHODS + File.separator +
                JACGCallGraphFileUtil.getCallGraphMethodFileName(entryCalleeSimpleClassName, entryMethodName, entryCalleeMethodHash) + JACGConstants.EXT_TXT;
        logger.info("当前方法输出文件名 {}", outputFilePath4Method);

        // 判断文件是否生成过
        if (!writtenFileNameSet.add(outputFilePath4Method)) {
            logger.info("当前文件已生成过，不再处理 {} {} {}", origTaskText, entryCalleeFullMethod, outputFilePath4Method);
            return true;
        }

        try (BufferedWriter writer4Method = JavaCGFileUtil.genBufferedWriter(outputFilePath4Method)) {
            // 判断配置文件中是否已指定忽略当前方法
            if (ignoreCurrentMethod(null, entryCalleeFullMethod)) {
                logger.info("配置文件中已指定忽略当前方法，不处理 {}", entryCalleeFullMethod);
                return true;
            }

            // 记录一个被调用方法的调用链信息
            return recordOneCalleeMethod(entryCalleeSimpleClassName, entryCalleeMethodHash, entryCalleeFullMethod, findMethodTaskElement.getReturnType(),
                    findMethodTaskElement.getCallFlags(), writer4Method);
        } catch (Exception e) {
            logger.error("error {} {} ", entryCalleeSimpleClassName, outputFilePath4Method, e);
            return false;
        }
    }

    // 记录一个被调用方法的调用链信息
    private boolean recordOneCalleeMethod(String entryCalleeSimpleClassName,
                                          String entryCalleeMethodHash,
                                          String entryCalleeFullMethod,
                                          String entryCalleeReturnType,
                                          int callFlags,
                                          BufferedWriter writer4Method) throws IOException {
        StringBuilder calleeInfo = new StringBuilder();

        // 在文件第1行写入当前方法的完整信息
        calleeInfo.append(entryCalleeFullMethod).append(JavaCGConstants.NEW_LINE);

        // 确定写入输出文件的当前调用方法信息
        String entryCalleeInfo = chooseEntryCalleeInfo(entryCalleeSimpleClassName, entryCalleeFullMethod, entryCalleeReturnType);

        // 第2行写入当前方法的信息
        calleeInfo.append(JACGCallGraphFileUtil.genOutputPrefix(JACGConstants.CALL_GRAPH_METHOD_LEVEL_START)).append(entryCalleeInfo);

        // 判断被调用方法上是否有注解
        if (MethodCallFlagsEnum.MCFE_EE_METHOD_ANNOTATION.checkFlag(callFlags)) {
            StringBuilder methodAnnotations = new StringBuilder();
            // 添加方法注解信息
            getMethodAnnotationInfo(entryCalleeFullMethod, entryCalleeMethodHash, methodAnnotations);
            if (methodAnnotations.length() > 0) {
                calleeInfo.append(methodAnnotations);
            }
        }

        if (businessDataTypeSet.contains(DefaultBusinessDataTypeEnum.BDTE_METHOD_ARG_GENERICS_TYPE.getType())) {
            // 显示方法参数泛型类型
            if (!addMethodArgGenericsTypeInfo(true, callFlags, entryCalleeMethodHash, calleeInfo)) {
                return false;
            }
        }
        if (businessDataTypeSet.contains(DefaultBusinessDataTypeEnum.BDTE_METHOD_RETURN_GENERICS_TYPE.getType())) {
            // 显示方法返回泛型类型
            if (!addMethodReturnGenericsTypeInfo(true, callFlags, entryCalleeMethodHash, calleeInfo)) {
                return false;
            }
        }

        calleeInfo.append(JavaCGConstants.NEW_LINE);

        // 记录查找到的调用方法信息List
        List<Pair<String, Boolean>> entryCallerMethodList = new ArrayList<>(JavaCGConstants.SIZE_100);

        // 根据指定的调用者方法HASH，查找所有被调用的方法信息
        if (!genAllGraph4Callee(entryCalleeMethodHash, entryCallerMethodList, entryCalleeFullMethod)) {
            return false;
        }

        // 记录所有的调用方法
        for (Pair<String, Boolean> pair : entryCallerMethodList) {
            calleeInfo.append(pair.getLeft());
            if (Boolean.TRUE.equals(pair.getRight())) {
                // 对于入口方法，写入标志
                calleeInfo.append(JACGConstants.CALLEE_FLAG_ENTRY);
            }
            calleeInfo.append(JavaCGConstants.NEW_LINE);
        }

        writer4Method.write(calleeInfo.toString());
        return true;
    }

    // 通过方法名查找对应的方法并处理
    private boolean handleOneCalleeMethodByName(String calleeSimpleClassName, List<FindMethodTaskElement> findMethodTaskElementList, String origTaskText,
                                                String methodInfoInTask) {
        List<FindMethodTaskElement> usedFindMethodTaskElementList = new ArrayList<>();

        // 遍历从数据库中查找到的方法信息，查找当前指定的方法名称或参数匹配的方法
        for (FindMethodTaskElement findMethodTaskElement : findMethodTaskElementList) {
            String methodNameAndArgs = JACGClassMethodUtil.getMethodNameWithArgsFromFull(findMethodTaskElement.getFullMethod());
            if (StringUtils.startsWith(methodNameAndArgs, methodInfoInTask)) {
                usedFindMethodTaskElementList.add(findMethodTaskElement);
            }
        }

        if (usedFindMethodTaskElementList.isEmpty()) {
            // 未查找到匹配的方法，生成空文件
            return genEmptyFile(calleeSimpleClassName, methodInfoInTask);
        }

        for (FindMethodTaskElement findMethodTaskElement : usedFindMethodTaskElementList) {
            // 处理一个被调用方法
            handleOneCalleeMethod(calleeSimpleClassName, findMethodTaskElement, origTaskText);
        }
        return true;
    }

    // 通过代码行号查找对应的方法并处理
    private boolean handleOneCalleeMethodByLineNumber(String calleeSimpleClassName, String origTaskText, String methodInfoInTask) {
        int methodLineNum = Integer.parseInt(methodInfoInTask);

        // 通过代码行号获取对应方法
        FindMethodTaskInfo findMethodTaskInfo = findMethodByLineNumber(true, calleeSimpleClassName, methodLineNum);
        if (findMethodTaskInfo.isError()) {
            // 返回处理失败
            return false;
        }

        if (findMethodTaskInfo.isGenEmptyFile()) {
            // 需要生成空文件
            return genEmptyFile(calleeSimpleClassName, methodInfoInTask);
        }

        List<FindMethodTaskElement> taskElementList = findMethodTaskInfo.getTaskElementList();
        for (FindMethodTaskElement findMethodTaskElement : taskElementList) {
            // 处理一个被调用方法
            handleOneCalleeMethod(calleeSimpleClassName, findMethodTaskElement, origTaskText);
        }
        return true;
    }

    // 生成空文件
    private boolean genEmptyFile(String calleeSimpleClassName, String methodInfoInTask) {
        // 生成内容为空的调用链文件名
        String outputFilePath4EmptyFile = currentOutputDirPath + File.separator + JACGConstants.DIR_OUTPUT_METHODS + File.separator +
                JACGCallGraphFileUtil.getEmptyCallGraphFileName(calleeSimpleClassName, methodInfoInTask);
        logger.info("生成空文件 {} {} {}", calleeSimpleClassName, methodInfoInTask, outputFilePath4EmptyFile);
        // 创建文件
        return JACGFileUtil.createNewFile(outputFilePath4EmptyFile);
    }

    /**
     * 根据指定的被调用者方法HASH，查找所有调用方法信息
     *
     * @param entryCalleeMethodHash
     * @param entryCallerMethodList
     * @param entryCalleeFullMethod
     * @return
     */
    protected boolean genAllGraph4Callee(String entryCalleeMethodHash,
                                         List<Pair<String, Boolean>> entryCallerMethodList,
                                         String entryCalleeFullMethod) {
        // 记录当前处理的方法调用信息的栈
        ListAsStack<CallGraphNode4Callee> callGraphNode4CalleeStack = new ListAsStack<>();
        // 记录父类方法调用子类方法对应信息的栈
        ListAsStack<SuperCallChildInfo> superCallChildInfoStack = new ListAsStack<>();

        // 初始加入最下层节点，callerMethodHash设为null
        CallGraphNode4Callee callGraphNode4CalleeHead = new CallGraphNode4Callee(entryCalleeMethodHash, null, entryCalleeFullMethod);
        callGraphNode4CalleeStack.push(callGraphNode4CalleeHead);

        // 输出结果数量
        int recordNum = 0;
        while (true) {
            // 从栈顶获取当前正在处理的节点
            CallGraphNode4Callee callGraphNode4Callee = callGraphNode4CalleeStack.peek();

            // 查询当前节点的一个上层调用方法
            WriteDbData4MethodCall callerMethod = queryOneCallerMethod(callGraphNode4Callee);
            if (callerMethod == null) {
                // 查询到调用方法为空时的处理
                if (handleCallerEmptyResult(callGraphNode4CalleeStack, superCallChildInfoStack, entryCallerMethodList)) {
                    return true;
                }
                continue;
            }

            // 查询到记录
            if (++recordNum % JACGConstants.NOTICE_LINE_NUM == 0) {
                logger.info("记录数达到 {} {}", recordNum, entryCalleeFullMethod);
            }

            String calleeFullMethod = callGraphNode4Callee.getCalleeFullMethod();
            String callerFullMethod = callerMethod.getCallerFullMethod();
            String origCallerMethodHash = callerMethod.getCallerMethodHash();
            int methodCallId = callerMethod.getCallId();
            int enabled = callerMethod.getEnabled();
            String callType = callerMethod.getCallType();

            // 处理父类方法调用子类方法的相关信息
            MethodAndHash callerMethodAndHash = handleSuperCallChildInfo(superCallChildInfoStack, callGraphNode4CalleeStack.getHead(), calleeFullMethod, callerFullMethod,
                    callType, origCallerMethodHash);
            if (callerMethodAndHash == null) {
                // 处理失败
                return false;
            }

            callerFullMethod = callerMethodAndHash.getFullMethod();
            String callerMethodHash = callerMethodAndHash.getMethodHash();

            // 处理被忽略的方法
            if (handleIgnoredMethod(callType, callerFullMethod, callerMethodHash, callGraphNode4CalleeStack, enabled, methodCallId)) {
                continue;
            }

            // 检查是否出现循环调用
            int back2Level = checkCycleCall(callGraphNode4CalleeStack, callerMethodHash, callerFullMethod);

            // 记录调用方法信息
            Pair<String, Boolean> pair = recordCallerInfo(callerFullMethod, callerMethod.getCallerReturnType(), methodCallId, callerMethod.getCallFlags(), callType,
                    callerMethod.getCallerLineNumber(), callGraphNode4CalleeStack.getHead(), callerMethodHash, back2Level);
            entryCallerMethodList.add(pair);

            // 记录可能出现一对多的方法调用
            if (!recordMethodCallMayBeMulti(methodCallId, callType)) {
                return false;
            }

            // 更新当前处理节点的callerMethodHash，使用原始调用方法HASH+长度
            callGraphNode4CalleeStack.peek().setCallerMethodHash(origCallerMethodHash);

            if (back2Level != JACGConstants.NO_CYCLE_CALL_FLAG) {
                // 将当前处理的层级指定到循环调用的节点，不再往上处理调用方法
                continue;
            }

            // 继续上一层处理
            CallGraphNode4Callee nextCallGraphNode4Callee = new CallGraphNode4Callee(callerMethodHash, null, callerFullMethod);
            callGraphNode4CalleeStack.push(nextCallGraphNode4Callee);
        }
    }

    /**
     * 查询到调用方法为空时的处理
     *
     * @param callGraphNode4CalleeStack
     * @param superCallChildInfoStack
     * @param entryCallerMethodList
     * @return true: 需要结束循环 false: 不结束循环
     */
    private boolean handleCallerEmptyResult(ListAsStack<CallGraphNode4Callee> callGraphNode4CalleeStack,
                                            ListAsStack<SuperCallChildInfo> superCallChildInfoStack,
                                            List<Pair<String, Boolean>> entryCallerMethodList) {
        if (callGraphNode4CalleeStack.atBottom()) {
            // 当前处理的节点为最下层节点，结束循环
            // 将调用方法列表中最后一条记录设置为入口方法
            markMethodAsEntry(entryCallerMethodList);
            return true;
        }

        if (!superCallChildInfoStack.isEmpty()) {
            // 记录父类方法调用子类方法对应信息的栈非空
            SuperCallChildInfo topSuperCallChildInfo = superCallChildInfoStack.peek();
            if (topSuperCallChildInfo.getChildCalleeNodeLevel() == callGraphNode4CalleeStack.getHead()) {
                // 记录父类方法调用子类方法对应信息的栈顶元素，与方法调用节点栈出栈的级别相同，出栈
                superCallChildInfoStack.removeTop();
            }
        }

        // 当前处理的节点不是最下层节点，返回下一层处理，出栈
        callGraphNode4CalleeStack.removeTop();

        // 将调用方法列表中最后一条记录设置为入口方法
        markMethodAsEntry(entryCallerMethodList);
        return false;
    }

    // 处理父类方法调用子类方法的相关信息
    private MethodAndHash handleSuperCallChildInfo(ListAsStack<SuperCallChildInfo> superCallChildInfoStack,
                                                   int nodeLevel,
                                                   String calleeFullMethod,
                                                   String callerFullMethod,
                                                   String callType,
                                                   String callerMethodHash) {
        if (JavaCGCallTypeEnum.CTE_SUPER_CALL_CHILD.getType().equals(callType)) {
            // 当前方法调用类型是父类调用子类方法，记录父类方法调用子类方法对应信息的栈入栈
            String calleeClassName = JACGClassMethodUtil.getClassNameFromMethod(calleeFullMethod);
            String calleeSimpleClassName = dbOperWrapper.getSimpleClassName(calleeClassName);
            SuperCallChildInfo superCallChildInfo = new SuperCallChildInfo(nodeLevel, calleeSimpleClassName, calleeClassName, calleeFullMethod);
            superCallChildInfoStack.push(superCallChildInfo);
            return new MethodAndHash(callerFullMethod, callerMethodHash);
        }

        // 获取子类的调用方法
        Pair<Boolean, MethodAndHash> pair = getSCCChildFullMethod(superCallChildInfoStack, callerFullMethod);
        if (Boolean.TRUE.equals(pair.getLeft())) {
            // 使用子类的调用方法
            return pair.getRight();
        }

        return new MethodAndHash(callerFullMethod, callerMethodHash);
    }

    /**
     * 获取子类的调用方法，若不满足则使用原始方法
     *
     * @param superCallChildInfoStack
     * @param callerFullMethod
     * @return left true: 使用子类的调用方法 false: 使用原始的调用方法
     * @return right: 子类的调用方法、方法HASH+长度
     */
    private Pair<Boolean, MethodAndHash> getSCCChildFullMethod(ListAsStack<SuperCallChildInfo> superCallChildInfoStack, String callerFullMethod) {
        // 判断父类方法调用子类方法对应信息的栈是否有数据
        if (superCallChildInfoStack.isEmpty()) {
            return new ImmutablePair<>(Boolean.FALSE, null);
        }

        String callerMethodWithArgs = JACGClassMethodUtil.getMethodNameWithArgsFromFull(callerFullMethod);
        if (callerMethodWithArgs.startsWith(JavaCGCommonNameConstants.METHOD_NAME_INIT)) {
            // 调用方法为构造函数，使用原始调用方法
            return new ImmutablePair<>(Boolean.FALSE, null);
        }

        String callerClassName = JACGClassMethodUtil.getClassNameFromMethod(callerFullMethod);
        String callerSimpleClassName = dbOperWrapper.getSimpleClassName(callerClassName);

        String sccChildFullMethod = null;
        String sccChildMethodHash = null;
        // 保存上一次处理的被调用唯一类名
        String lastChildCalleeSimpleClassName = null;
        // 对父类方法调用子类方法对应信息的栈，从栈顶往下遍历
        for (int i = superCallChildInfoStack.getHead(); i >= 0; i--) {
            SuperCallChildInfo superCallChildSInfo = superCallChildInfoStack.getElement(i);
            String childCalleeSimpleClassName = superCallChildSInfo.getChildCalleeSimpleClassName();

            if (lastChildCalleeSimpleClassName != null) {
                if (!jacgExtendsImplHandler.checkExtendsOrImplBySimple(lastChildCalleeSimpleClassName, childCalleeSimpleClassName)) {
                    // 当前已不是第一次处理，判断上次的子类是否为当前子类的父类，若是则可以继续处理，若否则结束循环
                    break;
                }
                logger.debug("继续处理子类 {} {}", lastChildCalleeSimpleClassName, childCalleeSimpleClassName);
            }
            lastChildCalleeSimpleClassName = childCalleeSimpleClassName;

            // 判断父类方法调用子类方法对应信息的栈的调用类（对应子类）是否为当前调用类的子类
            if (!jacgExtendsImplHandler.checkExtendsOrImplBySimple(callerSimpleClassName, childCalleeSimpleClassName)) {
                // 父类方法调用子类方法对应信息的栈的调用类（对应子类）不是当前被调用类的子类
                break;
            }
            // 父类方法调用子类方法对应信息的栈的调用类为当前被调用类的子类
            String tmpSccChildFullMethod = JavaCGMethodUtil.formatFullMethodWithArgs(superCallChildSInfo.getChildCalleeClassName(), callerMethodWithArgs);

            // 判断子类方法是否有被调用方法
            SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MI_QUERY_SIMPLE_CLASS_NAME;
            String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
            if (sql == null) {
                sql = " select " + DC.MI_SIMPLE_CLASS_NAME +
                        " from " + DbTableInfoEnum.DTIE_METHOD_INFO.getTableName() +
                        " where " + DC.MI_SIMPLE_CLASS_NAME + " = ?" +
                        " and " + DC.MI_FULL_METHOD + " like concat(?, '%')" +
                        " limit 1";
                sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
            }
            String simpleClassName = dbOperator.queryObjectOneColumn(sql, String.class, childCalleeSimpleClassName, tmpSccChildFullMethod);
            if (simpleClassName != null) {
                // 子类方法存在，需要继续使用栈中的数据进行处理
                continue;
            }

            // 子类方法存在，使用子类方法
            sccChildFullMethod = tmpSccChildFullMethod;
            sccChildMethodHash = JACGUtil.genHashWithLen(sccChildFullMethod);
        }

        if (sccChildFullMethod != null && sccChildMethodHash != null) {
            logger.debug("替换子类的向上的方法调用 {} {}", callerFullMethod, sccChildFullMethod);
            // 使用子类对应的方法，返回子类方法及子类方法HASH+长度
            return new ImmutablePair<>(Boolean.TRUE, new MethodAndHash(sccChildFullMethod, sccChildMethodHash));
        }
        // 使用原始被调用方法
        return new ImmutablePair<>(Boolean.FALSE, null);
    }

    /**
     * 处理被忽略的方法
     *
     * @param callType
     * @param callerFullMethod
     * @param callerMethodHash
     * @param callGraphNode4CalleeStack
     * @param enabled
     * @param methodCallId
     * @return true: 当前方法需要忽略 false: 当前方法不需要忽略
     */
    private boolean handleIgnoredMethod(String callType,
                                        String callerFullMethod,
                                        String callerMethodHash,
                                        ListAsStack<CallGraphNode4Callee> callGraphNode4CalleeStack,
                                        int enabled,
                                        int methodCallId) {
        // 判断是否需要忽略
        if (ignoreCurrentMethod(callType, callerFullMethod) || !JavaCGYesNoEnum.isYes(enabled)) {
            // 当前记录需要忽略
            // 更新当前处理节点的调用者方法HASH
            callGraphNode4CalleeStack.peek().setCallerMethodHash(callerMethodHash);

            if (!JavaCGYesNoEnum.isYes(enabled)) {
                // 记录被禁用的方法调用
                recordDisabledMethodCall(methodCallId, callType);
            }
            return true;
        }
        return false;
    }

    /**
     * 检查是否出现循环调用
     *
     * @param callGraphNode4CalleeStack
     * @param callerMethodHash
     * @param callerFullMethod
     * @return -1: 未出现循环调用，非-1: 出现循环调用，值为发生循环调用的层级
     */
    private int checkCycleCall(ListAsStack<CallGraphNode4Callee> callGraphNode4CalleeStack,
                               String callerMethodHash,
                               String callerFullMethod) {
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
        // 循环调用的日志信息
        StringBuilder cycleCallLogInfo = new StringBuilder();

        int cycleCallLevel = JACGConstants.NO_CYCLE_CALL_FLAG;
        for (int i = callGraphNode4CalleeStack.getHead(); i >= 0; i--) {
            CallGraphNode4Callee callGraphNode4Callee = callGraphNode4CalleeStack.getElement(i);
            if (callerMethodHash.equals(callGraphNode4Callee.getCalleeMethodHash())) {
                // 找到循环调用
                cycleCallLevel = i;
                break;
            }
        }

        // 每个层级的调用方法遍历完之后的处理
        if (cycleCallLevel != JACGConstants.NO_CYCLE_CALL_FLAG) {
            // 显示被循环调用的信息
            cycleCallLogInfo.append(JACGCallGraphFileUtil.genCycleCallFlag(cycleCallLevel))
                    .append(" ")
                    .append(callerFullMethod);
            // 记录循环调用信息
            for (int i = callGraphNode4CalleeStack.getHead(); i >= 0; i--) {
                CallGraphNode4Callee callGraphNode4Callee = callGraphNode4CalleeStack.getElement(i);
                if (cycleCallLogInfo.length() > 0) {
                    cycleCallLogInfo.append("\n");
                }
                cycleCallLogInfo.append(JACGCallGraphFileUtil.genOutputLevelFlag(i))
                        .append(" ")
                        .append(callGraphNode4Callee.getCalleeFullMethod());
            }
            logger.info("找到循环调用的方法\n{}", cycleCallLogInfo);
        }

        return cycleCallLevel;
    }

    // 将调用方法列表中最后一条记录设置为入口方法
    private void markMethodAsEntry(List<Pair<String, Boolean>> callerMethodList) {
        if (!JavaCGUtil.isCollectionEmpty(callerMethodList)) {
            Pair<String, Boolean> pair = callerMethodList.get(callerMethodList.size() - 1);
            pair.setValue(Boolean.TRUE);
        }
    }

    // 查询当前节点的一个上层调用方法
    private WriteDbData4MethodCall queryOneCallerMethod(CallGraphNode4Callee callGraphNode4Callee) {
        // 确定通过调用方法进行查询使用的SQL语句
        String sql = chooseQueryByCalleeMethodSql(callGraphNode4Callee.getCallerMethodHash());

        if (callGraphNode4Callee.getCallerMethodHash() == null) {
            // 第一次查询
            return dbOperator.queryObject(sql, WriteDbData4MethodCall.class, callGraphNode4Callee.getCalleeMethodHash());
        }
        // 不是第一次查询
        return dbOperator.queryObject(sql, WriteDbData4MethodCall.class, callGraphNode4Callee.getCalleeMethodHash(), callGraphNode4Callee.getCallerMethodHash());
    }

    // 确定通过调用方法进行查询使用的SQL语句
    protected String chooseQueryByCalleeMethodSql(String callerMethodHash) {
        if (callerMethodHash == null) {
            // 第一次查询
            SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MC_QUERY_ONE_CALLER1;
            String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
            if (sql == null) {
                // 确定查询被调用关系时所需字段
                sql = "select " + chooseCallerColumns() +
                        " from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName() +
                        " where " + DC.MC_CALLEE_METHOD_HASH + " = ?" +
                        " order by " + DC.MC_CALLER_METHOD_HASH +
                        " limit 1";
                sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
            }
            return sql;
        }

        // 不是第一次查询
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MC_QUERY_ONE_CALLER2;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            // 确定查询被调用关系时所需字段
            sql = "select " + chooseCallerColumns() +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName() +
                    " where " + DC.MC_CALLEE_METHOD_HASH + " = ?" +
                    " and " + DC.MC_CALLER_METHOD_HASH + " > ?" +
                    " order by " + DC.MC_CALLER_METHOD_HASH +
                    " limit 1";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return sql;
    }

    // 记录调用方法信息
    protected Pair<String, Boolean> recordCallerInfo(String callerFullMethod,
                                                     String callerReturnType,
                                                     int methodCallId,
                                                     int callFlags,
                                                     String callType,
                                                     int callerLineNum,
                                                     int currentNodeLevel,
                                                     String callerMethodHash,
                                                     int back2Level) {
        String callerClassName = JACGClassMethodUtil.getClassNameFromMethod(callerFullMethod);
        String callerSimpleClassName = dbOperWrapper.getSimpleClassName(callerClassName);

        StringBuilder callerInfo = new StringBuilder();
        callerInfo.append(JACGCallGraphFileUtil.genOutputPrefix(currentNodeLevel + 1));

        if (OutputDetailEnum.ODE_0 == outputDetailEnum) {
            // # 0: 展示 完整类名+方法名+方法参数+返回类型
            callerInfo.append(JACGClassMethodUtil.genFullMethodWithReturnType(callerFullMethod, callerReturnType));
        } else if (OutputDetailEnum.ODE_1 == outputDetailEnum) {
            // # 1: 展示 完整类名+方法名+方法参数
            callerInfo.append(callerFullMethod);
        } else if (OutputDetailEnum.ODE_2 == outputDetailEnum) {
            // # 2: 展示 完整类名+方法名
            String callerMethodName = JACGClassMethodUtil.getMethodNameFromFull(callerFullMethod);
            callerInfo.append(callerClassName)
                    .append(JavaCGConstants.FLAG_COLON)
                    .append(callerMethodName);
        } else {
            // # 3: 展示 简单类名（对于同名类展示完整类名）+方法名
            String callerMethodName = JACGClassMethodUtil.getMethodNameFromFull(callerFullMethod);
            callerInfo.append(callerSimpleClassName)
                    .append(JavaCGConstants.FLAG_COLON)
                    .append(callerMethodName);
        }

        // 判断调用方法上是否有注解
        Map<String, Map<String, BaseAnnotationAttribute>> methodAnnotationMap = null;
        if (MethodCallFlagsEnum.MCFE_ER_METHOD_ANNOTATION.checkFlag(callFlags)) {
            StringBuilder methodAnnotations = new StringBuilder();
            // 添加方法注解信息
            methodAnnotationMap = getMethodAnnotationInfo(callerFullMethod, callerMethodHash, methodAnnotations);
            if (methodAnnotations.length() > 0) {
                callerInfo.append(methodAnnotations);
            }
        }

        // 显示调用者代码行号
        callerInfo.append(JACGConstants.FLAG_TAB)
                .append(JavaCGConstants.FLAG_LEFT_BRACKET)
                .append(callerSimpleClassName)
                .append(JavaCGConstants.FLAG_COLON)
                .append(callerLineNum)
                .append(JavaCGConstants.FLAG_RIGHT_BRACKET);

        // 添加方法调用业务功能数据
        if (!addBusinessData(methodCallId, callFlags, callerMethodHash, callerInfo)) {
            return null;
        }

        // 为方法调用信息增加是否在其他线程执行标志
        addRunInOtherThread(callerInfo, methodCallId, callType, methodAnnotationMap);

        // 为方法调用信息增加是否在事务中执行标志
        addRunInTransaction(callerInfo, methodCallId, callType, methodAnnotationMap);

        // 添加循环调用标志
        if (back2Level != JACGConstants.NO_CYCLE_CALL_FLAG) {
            callerInfo.append(JACGConstants.FLAG_TAB).append(JACGCallGraphFileUtil.genCycleCallFlag(back2Level));
        }

        return new MutablePair<>(callerInfo.toString(), Boolean.FALSE);
    }

    // 确定写入输出文件的当前被调用方法信息
    private String chooseEntryCalleeInfo(String calleeSimpleClassName, String calleeFullMethod, String entryCalleeReturnType) {
        if (OutputDetailEnum.ODE_0 == outputDetailEnum) {
            // # 0: 展示 完整类名+方法名+方法参数+返回类型
            return JACGClassMethodUtil.genFullMethodWithReturnType(calleeFullMethod, entryCalleeReturnType);
        }
        if (OutputDetailEnum.ODE_1 == outputDetailEnum) {
            // # 1: 展示 完整类名+方法名+方法参数
            return calleeFullMethod;
        } else if (OutputDetailEnum.ODE_2 == outputDetailEnum) {
            // # 2: 展示 完整类名+方法名
            String calleeClassName = JACGClassMethodUtil.getClassNameFromMethod(calleeFullMethod);
            String calleeMethodName = JACGClassMethodUtil.getMethodNameFromFull(calleeFullMethod);
            return JACGClassMethodUtil.getClassAndMethodName(calleeClassName, calleeMethodName);
        }
        // # 3: 展示 简单类名（对于同名类展示完整类名）+方法名
        String calleeMethodName = JACGClassMethodUtil.getMethodNameFromFull(calleeFullMethod);
        return JACGClassMethodUtil.getClassAndMethodName(calleeSimpleClassName, calleeMethodName);
    }

    // 确定查询被调用关系时所需字段
    private String chooseCallerColumns() {
        return JACGSqlUtil.joinColumns(
                DC.MC_CALL_ID,
                DC.MC_CALL_TYPE,
                DC.MC_ENABLED,
                DC.MC_CALLER_METHOD_HASH,
                DC.MC_CALLER_FULL_METHOD,
                DC.MC_CALLER_LINE_NUMBER,
                DC.MC_CALLER_RETURN_TYPE,
                DC.MC_CALL_FLAGS
        );
    }

    // 打印存在一对多的方法调用，自定义处理
    @Override
    protected void printMultiMethodCallCustom(String callerMethodHash, MarkdownWriter markdownWriter) throws IOException {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MC_QUERY_ALL_CALLER;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select distinct(" + DC.MC_CALLER_FULL_METHOD + ")" +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName() +
                    " where " + DC.MC_CALLEE_METHOD_HASH + " = ?" +
                    " order by " + DC.MC_CALLER_FULL_METHOD;
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        List<String> list = dbOperator.queryListOneColumn(sql, String.class, callerMethodHash);
        if (list == null) {
            logger.error("查询所有的调用方法失败 {}", callerMethodHash);
            return;
        }

        if (list.size() <= 1) {
            return;
        }

        markdownWriter.addListWithNewLine(DC.MC_CALLEE_METHOD_HASH);
        markdownWriter.addLineWithNewLine(callerMethodHash);
        markdownWriter.addListWithNewLine(DC.MC_CALLER_FULL_METHOD + "（调用方法）");
        markdownWriter.addCodeBlock();
        for (String callerMethod : list) {
            markdownWriter.addLine(callerMethod);
        }
        markdownWriter.addCodeBlock();
    }

    // 将输出的方法文件合并为类对应的文件
    private void combineClassFile() {
        List<File> methodOutputFileList = JACGFileUtil.findFileInCurrentDir(currentOutputDirPath + File.separator + JACGConstants.DIR_OUTPUT_METHODS, JACGConstants.EXT_TXT);
        if (JavaCGUtil.isCollectionEmpty(methodOutputFileList)) {
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

            // 从方法对应的调用链文件名中获取对应的类名
            String className = JACGCallGraphFileUtil.getClassNameFromMethodFileName(methodOutputFileName);
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
        String classFilePath = currentOutputDirPath + File.separator + lastClassName + JACGConstants.EXT_TXT;
        logger.info("将以下类对应的方法文件合并为类对应的文件 {}", classFilePath);
        JACGFileUtil.combineTextFile(classFilePath, combineMethodFileList);
    }
}
