package com.adrninistrator.jacg.runner;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.DefaultBusinessDataTypeEnum;
import com.adrninistrator.jacg.common.enums.JACGMethodTypeEnum;
import com.adrninistrator.jacg.common.enums.MethodCallFlagsEnum;
import com.adrninistrator.jacg.common.enums.OutputDetailEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.conf.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.dto.callgraph.CallGraphJson;
import com.adrninistrator.jacg.dto.callgraph.CallGraphJsonMethod;
import com.adrninistrator.jacg.dto.callgraph.CallGraphJsonMethodCall;
import com.adrninistrator.jacg.dto.callgraph.CallGraphNode4Caller;
import com.adrninistrator.jacg.dto.callgraph.ChildCallSuperInfo;
import com.adrninistrator.jacg.dto.method.FullMethodWithReturnType;
import com.adrninistrator.jacg.dto.method.MethodAndHash;
import com.adrninistrator.jacg.dto.method.MethodDetail;
import com.adrninistrator.jacg.dto.method.MethodDetailNoReturnType;
import com.adrninistrator.jacg.dto.methodcall.MethodCallLineData;
import com.adrninistrator.jacg.dto.methodcall.MethodCallLineData4Er;
import com.adrninistrator.jacg.dto.task.CallerTaskInfo;
import com.adrninistrator.jacg.dto.task.FindMethodTaskElement;
import com.adrninistrator.jacg.dto.task.FindMethodTaskInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCall;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MyBatisMSWriteTable;
import com.adrninistrator.jacg.handler.dto.mybatis.MyBatisMSTableInfo;
import com.adrninistrator.jacg.runner.base.AbstractRunnerGenCallGraph;
import com.adrninistrator.jacg.util.JACGCallGraphFileUtil;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGFileUtil;
import com.adrninistrator.jacg.util.JACGJsonUtil;
import com.adrninistrator.javacg2.common.JavaCG2CommonNameConstants;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.common.enums.JavaCG2CallTypeEnum;
import com.adrninistrator.javacg2.common.enums.JavaCG2YesNoEnum;
import com.adrninistrator.javacg2.dto.counter.JavaCG2Counter;
import com.adrninistrator.javacg2.dto.stack.ListAsStack;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;
import com.adrninistrator.javacg2.util.JavaCG2FileUtil;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

/**
 * @author adrninistrator
 * @date 2021/6/18
 * @description: 生成指定方法向下完整调用链
 */

public class RunnerGenAllGraph4Caller extends AbstractRunnerGenCallGraph {
    private static final Logger logger = LoggerFactory.getLogger(RunnerGenAllGraph4Caller.class);

    // 简单类名及对应的完整类名Map
    private final Map<String, String> simpleAndClassNameMap = new ConcurrentHashMap<>();

    // 在一个调用方法中出现多次的被调用方法（包含方法调用业务功能数据），是否需要忽略
    private boolean ignoreDupCalleeInOneCaller;

    // 生成向下的方法调用链时，是否需要输出JSON格式的内容
    private boolean callGraphGenJsonCaller;

    // 当方法类型在以下Set中时，生成方法完整调用链时忽略
    private Set<String> ignoreMethodTypeSet;

    // 当前生成的完整方法调用链数据列表
    private List<MethodCallLineData4Er> allMethodCallLineData4ErList;

    public RunnerGenAllGraph4Caller() {
        super();
    }

    public RunnerGenAllGraph4Caller(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
    }

    @Override
    public boolean preHandle() {
        // 公共预处理
        if (!commonPreHandle()) {
            return false;
        }

        // 读取配置文件中指定的需要处理的任务
        if (!readTaskInfo(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER)) {
            return false;
        }

        // 创建输出文件所在目录
        if (!createOutputDir(JACGConstants.DIR_OUTPUT_GRAPH_FOR_CALLER)) {
            return false;
        }

        ignoreDupCalleeInOneCaller = configureWrapper.getMainConfig(ConfigKeyEnum.CKE_IGNORE_DUP_CALLEE_IN_ONE_CALLER);
        callGraphGenJsonCaller = configureWrapper.getMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_GEN_JSON_CALLER);

        if (callGraphGenJsonCaller && !callGraphWriteToFile) {
            logger.error("{}={} 时 {}={} 不会生效", ConfigKeyEnum.CKE_CALL_GRAPH_GEN_JSON_CALLER.getKey(), Boolean.FALSE, ConfigKeyEnum.CKE_CALL_GRAPH_WRITE_TO_FILE.getKey(),
                    Boolean.TRUE);
            return false;
        }

        ignoreMethodTypeSet = configureWrapper.getOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_IGNORE_METHOD_TYPE_4CALLER, true);
        if (ignoreMethodTypeSet == null) {
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
        }
    }

    // 执行实际处理
    private boolean operate() {
        // 生成文件中指定的需要执行的任务信息
        List<CallerTaskInfo> callerTaskInfoList = genCallerTaskInfo();
        if (callerTaskInfoList == null) {
            logger.error("执行失败，请检查配置文件 {} 的内容", OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER);
            return false;
        }
        if (callerTaskInfoList.isEmpty()) {
            logger.warn("查询需要执行的任务为空");
            return true;
        }

        if (callGraphReturnInMemory) {
            // 需要在内存中返回调用链数据
            if (callerTaskInfoList.size() > 1) {
                noticeReturnInMemoryMultiTask(callerTaskInfoList);
                return false;
            }
            allMethodCallLineData4ErList = new ArrayList<>();
        }

        // 创建线程
        createThreadPoolExecutor(callerTaskInfoList.size());

        // 遍历需要处理的任务，执行任务并等待
        for (CallerTaskInfo callerTaskInfo : callerTaskInfoList) {
            try {
                // 处理一个调用方法
                if (!handleOneCaller(callerTaskInfo)) {
                    // 等待直到任务执行完毕
                    wait4TPEDone();
                    // 记录执行失败的任务信息
                    recordTaskFail(callerTaskInfo.getOrigText());
                }
            } catch (Exception e) {
                logger.error("error {} ", JACGJsonUtil.getJsonStr(callerTaskInfo), e);
                // 记录执行失败的任务信息
                recordTaskFail(callerTaskInfo.getOrigText());
            }
        }

        // 等待直到任务执行完毕
        wait4TPEDone();
        return true;
    }

    // 生成需要执行的任务信息
    private List<CallerTaskInfo> genCallerTaskInfo() {
        Set<String> handledClassNameSet = new HashSet<>();

        List<CallerTaskInfo> callerTaskInfoList = new ArrayList<>(taskSet.size());
        for (String task : taskSet) {
            if (!StringUtils.containsAny(task, JACGConstants.FLAG_SPACE, JavaCG2Constants.FLAG_COLON)) {
                // 当前任务不包含空格或冒号，说明需要将一个类的全部方法添加至任务中
                if (!addAllMethodsInClass2Task(task, handledClassNameSet, callerTaskInfoList)) {
                    return null;
                }
                continue;
            }

            String left = task;
            int lineNumStart = JACGConstants.LINE_NUM_NONE;
            int lineNumEnd = JACGConstants.LINE_NUM_NONE;

            if (task.contains(JACGConstants.FLAG_SPACE)) {
                String[] array = StringUtils.splitPreserveAllTokens(task, JACGConstants.FLAG_SPACE);
                if (array.length != 2) {
                    logger.error("指定的类名+方法名非法，格式应为 {类名}:{方法名/方法中的代码行号} {起始代码行号}-{结束代码行号} {}", task);
                    return null;
                }

                left = array[0];
                String right = array[1];
                String[] arrayRight = StringUtils.splitPreserveAllTokens(right, JACGConstants.FLAG_MINUS);
                if (arrayRight.length != 2) {
                    logger.error("指定的行号非法，格式应为 {起始代码行号}-{结束代码行号} {}", task);
                    return null;
                }

                if (!JavaCG2Util.isNumStr(arrayRight[0]) || !JavaCG2Util.isNumStr(arrayRight[1])) {
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

            String[] arrayLeft = StringUtils.splitPreserveAllTokens(left, JavaCG2Constants.FLAG_COLON);
            if (arrayLeft.length != 2) {
                logger.error("配置文件 {} 中指定的任务信息非法 {} 允许使用的格式请参考对应配置文件", OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER, task);
                return null;
            }

            String callerClassName = arrayLeft[0];
            String arg2InTask = arrayLeft[1];

            if (StringUtils.isAnyBlank(callerClassName, arg2InTask)) {
                logger.error("配置文件 {} 中指定的任务信息非法 {} 指定的类名+方法名存在空值，允许使用的格式请参考对应配置文件", OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER, task);
                return null;
            }

            // 获取唯一类名（简单类名或完整类名）
            String simpleClassName = dbOperWrapper.querySimpleClassNameInTask(callerClassName);
            if (handledClassNameSet.contains(simpleClassName)) {
                logger.warn("当前类的全部方法已添加至任务，不需要再指定 {} {}", simpleClassName, task);
                continue;
            }

            CallerTaskInfo callerTaskInfo = new CallerTaskInfo();
            callerTaskInfo.setOrigText(task);
            callerTaskInfo.setCallerSimpleClassName(simpleClassName);
            if (JavaCG2Util.isNumStr(arg2InTask)) {
                // 任务中指定的第二个参数为全数字，作为代码行号处理
                callerTaskInfo.setMethodLineNumber(Integer.parseInt(arg2InTask));
            } else {
                // 任务中指定的第二个参数不是全数字，作为方法名称处理
                callerTaskInfo.setCallerMethodName(arg2InTask);
            }
            callerTaskInfo.setLineNumStart(lineNumStart);
            callerTaskInfo.setLineNumEnd(lineNumEnd);
            callerTaskInfo.setSaveDirPath(null);

            callerTaskInfoList.add(callerTaskInfo);
        }

        return callerTaskInfoList;
    }

    // 将一个类的全部方法添加至任务中
    private boolean addAllMethodsInClass2Task(String className, Set<String> handledClassNameSet, List<CallerTaskInfo> callerTaskInfoList) {
        // 获取唯一类名（简单类名或完整类名）
        String simpleClassName = dbOperWrapper.querySimpleClassNameInTask(className);
        if (!handledClassNameSet.add(simpleClassName)) {
            // 已处理过的类不再处理
            logger.warn("当前类已处理过，不需要再指定 {} {}", simpleClassName, className);
            return true;
        }

        // 查询当前类的所有方法
        List<FullMethodWithReturnType> methodList = dbOperWrapper.queryMethodByClassName(simpleClassName);
        if (methodList == null) {
            return false;
        }

        if (methodList.isEmpty()) {
            // 未找到指定类对应的方法
            CallerTaskInfo callerTaskInfo = new CallerTaskInfo();
            callerTaskInfo.setOrigText(className);
            callerTaskInfo.setCallerSimpleClassName(className);
            callerTaskInfoList.add(callerTaskInfo);
            return true;
        }

        for (FullMethodWithReturnType fullMethodWithReturnType : methodList) {
            String methodNameWithArgs = JACGClassMethodUtil.getMethodNameWithArgsFromFull(fullMethodWithReturnType.getFullMethod());
            CallerTaskInfo callerTaskInfo = new CallerTaskInfo();
            callerTaskInfo.setOrigText(null);
            callerTaskInfo.setCallerSimpleClassName(simpleClassName);
            callerTaskInfo.setCallerMethodName(methodNameWithArgs);
            callerTaskInfo.setCallerReturnType(fullMethodWithReturnType.getReturnType());
            callerTaskInfo.setLineNumStart(JACGConstants.LINE_NUM_NONE);
            callerTaskInfo.setLineNumEnd(JACGConstants.LINE_NUM_NONE);

            callerTaskInfoList.add(callerTaskInfo);
        }
        return true;
    }

    // 处理一个调用方法
    private boolean handleOneCaller(CallerTaskInfo callerTaskInfo) {
        String startCallerSimpleClassName = callerTaskInfo.getCallerSimpleClassName();

        // 获取调用方完整类名
        String startCallerClassName = getCallerClassName(startCallerSimpleClassName);
        if (StringUtils.isBlank(startCallerClassName)) {
            // 生成代表不存在的文件并返回成功
            return genNotFoundFile(callerTaskInfo);
        }

        FindMethodTaskInfo findMethodTaskInfo = null;
        if (callerTaskInfo.getCallerMethodName() != null) {
            // 通过方法名称获取调用方方法
            findMethodTaskInfo = findCallerMethodByName(startCallerClassName, callerTaskInfo);
        } else if (callerTaskInfo.getMethodLineNumber() != 0) {
            findMethodTaskInfo = findCallerMethodByLineNumber(callerTaskInfo);
        }

        if (findMethodTaskInfo == null || findMethodTaskInfo.isGenNotFoundFile()) {
            // 生成代表不存在的文件
            return genNotFoundFile(callerTaskInfo);
        }

        List<FindMethodTaskElement> taskElementList = findMethodTaskInfo.getTaskElementList();
        for (FindMethodTaskElement findMethodTaskElement : taskElementList) {
            // 等待直到允许任务执行
            wait4TPEAllowExecute();
            executeByTPE(() -> {
                try {
                    // 执行处理一个调用方法
                    if (!doHandleOneCaller(callerTaskInfo, findMethodTaskElement)) {
                        // 记录执行失败的任务信息
                        recordTaskFail(callerTaskInfo.getOrigText());
                    }
                } catch (Throwable e) {
                    logger.error("处理出现异常 {} ", callerTaskInfo.getOrigText(), e);
                    // 记录执行失败的任务信息
                    recordTaskFail(callerTaskInfo.getOrigText());
                }
            });
        }
        return true;
    }

    // 执行处理一个调用方法
    private boolean doHandleOneCaller(CallerTaskInfo callerTaskInfo, FindMethodTaskElement findMethodTaskElement) throws Exception {
        String startCallerMethodHash = findMethodTaskElement.getMethodHash();
        String startCallerFullMethod = findMethodTaskElement.getFullMethod();
        String startCallerSimpleClassName = callerTaskInfo.getCallerSimpleClassName();
        int startLineNumStart = callerTaskInfo.getLineNumStart();
        int startLineNumEnd = callerTaskInfo.getLineNumEnd();
        logger.info("找到入口方法 {} {}", startCallerMethodHash, startCallerFullMethod);

        JavaCG2Counter recordNumCounter = new JavaCG2Counter(0);
        BufferedWriter callGraphWriter = null;
        BufferedWriter callGraphJsonWriter = null;
        String outputFilePath = null;
        try {
            if (callGraphWriteToFile) {
                // 调用链数据需要生成文件
                // 获取当前实际的方法名，而不是使用文件中指定的方法名，文件中指定的方法名可能包含参数，会很长，不可控
                String startCallerMethodName = JavaCG2ClassMethodUtil.getMethodNameFromFull(startCallerFullMethod);
                // 确定当前方法对应输出文件路径，格式: 配置文件中指定的类名（简单类名或完整类名）+方法名+方法名hash.txt
                StringBuilder outputFilePathTail = new StringBuilder();
                if (callerTaskInfo.getSaveDirPath() != null) {
                    outputFilePathTail.append(callerTaskInfo.getSaveDirPath()).append(File.separator);
                    // 创建对应目录
                    if (!JavaCG2FileUtil.isDirectoryExists(outputFilePathTail.toString())) {
                        return false;
                    }
                }
                // CallerGraphBusinessDataExtractor.genBusinessDataFile()方法中有使用以下文件名格式，不能随便修改
                // 生成方法对应的调用链文件名
                String outputFileName = chooseCallGraphFileName(startCallerSimpleClassName, startCallerMethodName, startCallerMethodHash);
                outputFilePathTail.append(outputFileName);
                if (startLineNumStart != JACGConstants.LINE_NUM_NONE && startLineNumEnd != JACGConstants.LINE_NUM_NONE) {
                    // 假如有指定行号时，再加上：@{起始行号}-{结束行号}
                    outputFilePathTail.append(JACGConstants.FLAG_AT).append(startLineNumStart).append(JACGConstants.FLAG_MINUS).append(startLineNumEnd);
                }
                outputFilePath = currentOutputDirPath + File.separator + outputFilePathTail + JavaCG2Constants.EXT_TXT;
                logger.info("当前输出的调用链文件名 {} {}", outputFilePath, startCallerFullMethod);

                // 判断文件是否生成过
                if (!writtenFileNameSet.add(outputFilePath)) {
                    logger.info("当前文件已生成过，不再处理 {} {} {}", callerTaskInfo.getOrigText(), startCallerFullMethod, outputFilePath);
                    return true;
                }
                callGraphWriter = JavaCG2FileUtil.genBufferedWriter(outputFilePath);
                if (callGraphWriteToFile && callGraphGenJsonCaller) {
                    String outputJsonFilePath = currentOutputDirPath + File.separator + JACGConstants.DIR_OUTPUT_JSON + File.separator +
                            outputFilePathTail + JACGConstants.EXT_JSON;
                    logger.info("生成JSON格式的调用链文件 {}", outputJsonFilePath);
                    File outputJsonFile = new File(outputJsonFilePath);
                    // 创建对应目录
                    if (!JavaCG2FileUtil.isDirectoryExists(outputJsonFile.getParent())) {
                        return false;
                    }
                    callGraphJsonWriter = JavaCG2FileUtil.genBufferedWriter(outputJsonFilePath);
                }
            }
            // 判断配置文件中是否已指定忽略当前方法
            if (ignoreCurrentMethod(null, startCallerFullMethod)) {
                logger.info("配置文件中已指定忽略当前方法，不处理 {}", startCallerFullMethod);
                return true;
            }

            // 生成起始方法的当前行调用链数据
            MethodCallLineData4Er methodCallLineData4Er = new MethodCallLineData4Er(JACGConstants.CALL_GRAPH_METHOD_LEVEL_START, null, 0, startCallerFullMethod,
                    startCallerMethodHash, startCallerMethodHash, findMethodTaskElement.getReturnType(), 0, null, null);

            StringBuilder methodCallLineStr = new StringBuilder();
            // 在文件第1行写入当前方法的完整信息
            methodCallLineStr.append(startCallerFullMethod).append(JavaCG2Constants.NEW_LINE);

            // 生成起始调用方法信息（包含方法注解信息、方法调用业务功能数据）
            String startCallerInfo = genCalleeInfo(methodCallLineData4Er);
            if (startCallerInfo == null) {
                return false;
            }

            // 生成方法调用链每行数据字符串
            methodCallLineStr.append(genMethodCallLineStr(methodCallLineData4Er)).append(JavaCG2Constants.NEW_LINE);
            if (callGraphWriter != null) {
                callGraphWriter.write(methodCallLineStr.toString());
            }
            if (callGraphReturnInMemory) {
                // 方法调用链当前行的数据记录到用于返回的列表
                allMethodCallLineData4ErList.add(methodCallLineData4Er);
            }

            // 根据指定的调用方方法HASH，查找所有被调用的方法信息
            return genAllGraph4Caller(startCallerMethodHash, startCallerFullMethod, startLineNumStart, startLineNumEnd, callGraphWriter, callGraphJsonWriter, recordNumCounter);
        } finally {
            if (callGraphWriter != null) {
                IOUtils.closeQuietly(callGraphWriter);
                if (recordNumCounter.getCount() == 0) {
                    // 方法对应的调用链为空，修改调用链文件名为空文件
                    renameCallGraphFile2Empty(outputFilePath);
                }
            }
            if (callGraphJsonWriter != null) {
                IOUtils.closeQuietly(callGraphJsonWriter);
            }
        }
    }

    // 通过方法名称获取调用方方法
    private FindMethodTaskInfo findCallerMethodByName(String callerClassName, CallerTaskInfo callerTaskInfo) {
        String callerMethodNameInTask = callerTaskInfo.getCallerMethodName();
        String callerSimpleClassName = callerTaskInfo.getCallerSimpleClassName();
        String fullMethodPrefix = JACGClassMethodUtil.genClassAndMethodName(callerClassName, callerMethodNameInTask);

        Set<String> handledCallerMethodHashSet = new HashSet<>();
        FindMethodTaskInfo findMethodTaskInfo = FindMethodTaskInfo.genFindMethodInfoSuccess();
        // 通过方法名称获取调用方方法
        List<WriteDbData4MethodCall> callerMethodList = dbOperWrapper.queryCallerMethodByName(callerSimpleClassName, fullMethodPrefix);
        if (!JavaCG2Util.isCollectionEmpty(callerMethodList)) {
            // 遍历找到的方法
            for (WriteDbData4MethodCall callerMethod : callerMethodList) {
                if (handledCallerMethodHashSet.add(callerMethod.getCallerMethodHash())) {
                    findMethodTaskInfo.addTaskElement(callerMethod.getCallerMethodHash(), callerMethod.getCallerFullMethod(), callerMethod.getCallerReturnType());
                }
            }
            return findMethodTaskInfo;
        }

        // 从方法调用关系表未找到指定的调用方法，从方法信息表查询
        List<WriteDbData4MethodInfo> methodInfoList;
        if (callerTaskInfo.getCallerReturnType() == null) {
            methodInfoList = dbOperWrapper.queryMethodInfoByClassMethodPrefix(callerSimpleClassName, fullMethodPrefix);
        } else {
            methodInfoList = dbOperWrapper.queryMethodInfoByClassMethodPrefixReturnType(callerSimpleClassName, fullMethodPrefix, callerTaskInfo.getCallerReturnType());
        }
        if (!JavaCG2Util.isCollectionEmpty(methodInfoList)) {
            for (WriteDbData4MethodInfo methodInfo : methodInfoList) {
                if (handledCallerMethodHashSet.add(methodInfo.getMethodHash())) {
                    findMethodTaskInfo.addTaskElement(methodInfo.getMethodHash(), methodInfo.getFullMethod(), methodInfo.getReturnType());
                }
            }
            return findMethodTaskInfo;
        }

        logger.warn("未找到指定的调用方法 {} {}", callerSimpleClassName, fullMethodPrefix);
        return FindMethodTaskInfo.genFindMethodInfoGenNotFoundFile();
    }

    // 通过代码行号获取调用方方法
    private FindMethodTaskInfo findCallerMethodByLineNumber(CallerTaskInfo callerTaskInfo) {
        int methodLineNum = callerTaskInfo.getMethodLineNumber();
        String callerSimpleClassName = callerTaskInfo.getCallerSimpleClassName();
        // 通过代码行号获取对应方法
        return findMethodByLineNumber(false, callerSimpleClassName, methodLineNum);
    }

    // 生成代表任务中指定的类或方法不存在的文件
    private boolean genNotFoundFile(CallerTaskInfo callerTaskInfo) {
        if (!callGraphWriteToFile) {
            // 不生成文件
            return true;
        }
        StringBuilder emptyFileDirPath = new StringBuilder().append(currentOutputDirPath).append(File.separator);
        if (callerTaskInfo.getSaveDirPath() != null) {
            emptyFileDirPath.append(callerTaskInfo.getSaveDirPath()).append(File.separator);
            if (!JavaCG2FileUtil.isDirectoryExists(emptyFileDirPath.toString())) {
                return false;
            }
        }
        String emptyFilePath = emptyFileDirPath.append(JACGCallGraphFileUtil.genNotFoundGraphFileName(callerTaskInfo.getOrigText())).toString();
        logger.info("生成代表任务中指定的类或方法不存在的文件 {}", callerTaskInfo.getOrigText());
        return JACGFileUtil.createNewFile(emptyFilePath);
    }

    /**
     * 根据指定的调用方方法HASH，查找所有被调用方法信息
     *
     * @param startCallerMethodHash
     * @param startCallerFullMethod 仅代表调用当前方法时的调用方方法，不代表以下while循环中每次处理到的调用方方法
     * @param startLineNumStart
     * @param startLineNumEnd
     * @param callGraphWriter
     * @param callGraphJsonWriter
     * @param recordNumCounter
     * @return
     */
    private boolean genAllGraph4Caller(String startCallerMethodHash, String startCallerFullMethod, int startLineNumStart, int startLineNumEnd, BufferedWriter callGraphWriter,
                                       BufferedWriter callGraphJsonWriter, JavaCG2Counter recordNumCounter) throws IOException {
        // 方法调用链JSON格式
        CallGraphJson callGraphJson = null;
        if (callGraphGenJsonCaller) {
            callGraphJson = new CallGraphJson();
        }

        // 记录当前处理的方法调用信息的栈
        ListAsStack<CallGraphNode4Caller> callGraphNode4CallerStack = new ListAsStack<>();
        // 记录子类方法调用父类方法对应信息的栈
        ListAsStack<ChildCallSuperInfo> childCallSuperInfoStack = new ListAsStack<>();

        // 初始加入最上层节点，id设为0（方法调用关系表最小call_id为1）
        CallGraphNode4Caller callGraphNode4CallerHead = new CallGraphNode4Caller(startCallerMethodHash, JavaCG2Constants.METHOD_CALL_ID_MIN_BEFORE, startCallerFullMethod);
        callGraphNode4CallerStack.push(callGraphNode4CallerHead);

        // 是否需要显示方法调用被调用方法数
        boolean showCalleeMethodNum;

        // 记录各个层级的调用方法中有被调用过的方法（包含方法注解、方法调用业务功能数据）
        ListAsStack<Set<String>> recordedCalleeStack = null;
        if (ignoreDupCalleeInOneCaller) {
            recordedCalleeStack = new ListAsStack<>();
            // 为第0层的调用方添加Set，不能在以下while循环的if (callGraphNode4CallerStack.atBottom())中添加，因为会执行多次
            recordedCalleeStack.push(new HashSet<>());
        }

        // 记录需要写入文件的方法调用链内容
        List<MethodCallLineData4Er> methodCallLineData4ErList = new ArrayList<>(JavaCG2Constants.SIZE_100);

        while (true) {
            int lineNumStart = JACGConstants.LINE_NUM_NONE;
            int lineNumEnd = JACGConstants.LINE_NUM_NONE;
            if (callGraphNode4CallerStack.atBottom()) {
                lineNumStart = startLineNumStart;
                lineNumEnd = startLineNumEnd;
            }

            // 从栈顶获取当前正在处理的节点
            CallGraphNode4Caller callGraphNode4Caller = callGraphNode4CallerStack.peek();
            // 查询当前节点的一个下层被调用方法
            WriteDbData4MethodCall methodCall = dbOperWrapper.queryOneCalleeMethod(callGraphNode4Caller, lineNumStart, lineNumEnd);
            if (methodCall == null) {
                // 查询到被调用方法为空时的处理
                if (handleCalleeEmptyResult(callGraphNode4CallerStack, childCallSuperInfoStack, recordedCalleeStack, callGraphNode4Caller.getCalleeMethodNum(), callGraphWriter,
                        methodCallLineData4ErList)) {
                    if (callGraphGenJsonCaller) {
                        // 生成JSON格式的调用链文件
                        String jsonData = JACGJsonUtil.getJsonStr(callGraphJson);
                        if (jsonData == null) {
                            logger.error("生成JSON格式的调用链失败");
                            return false;
                        }
                        callGraphJsonWriter.write(jsonData);
                    }
                    return true;
                }
                continue;
            }

            String callerFullMethod = callGraphNode4Caller.getCallerFullMethod();
            int methodCallId = methodCall.getCallId();
            String callType = methodCall.getCallType();
            int enabled = methodCall.getEnabled();
            String rawCalleeFullMethod = methodCall.getCalleeFullMethod();
            String rawCalleeMethodHash = methodCall.getCalleeMethodHash();

            // 处理子类方法调用父类方法的相关信息
            MethodAndHash calleeMethodAndHash = handleChildCallSuperInfo(childCallSuperInfoStack, callGraphNode4CallerStack.getHead(), rawCalleeFullMethod,
                    methodCall.getRawReturnType(), callerFullMethod, callType, rawCalleeMethodHash);
            if (calleeMethodAndHash == null) {
                // 处理失败
                return false;
            }

            String actualCalleeFullMethod = calleeMethodAndHash.getFullMethod();
            String actualCalleeMethodHash = calleeMethodAndHash.getMethodHash();
            int callFlags = methodCall.getCallFlags();

            // 处理被忽略的方法
            if (handleIgnoredMethod(callType, actualCalleeFullMethod, actualCalleeMethodHash, callGraphNode4CallerStack, enabled, methodCallId, callFlags)) {
                if (logger.isDebugEnabled()) {
                    logger.debug("当前被忽略的方法调用 {} {} -> {} {}", methodCall.getCallId(), callerFullMethod, actualCalleeFullMethod, methodCall.getCallerLineNumber());
                }
                continue;
            }

            // 当前记录需要处理
            // 获取方法调用层级
            int methodCallLevel = callGraphNode4CallerStack.getHead() + 1;
            // 生成方法调用链当前行的数据
            MethodCallLineData4Er methodCallLineData4Er = new MethodCallLineData4Er(methodCallLevel, methodCall.getCallerSimpleClassName(), methodCall.getCallerLineNumber(),
                    actualCalleeFullMethod, rawCalleeMethodHash, actualCalleeMethodHash, methodCall.getRawReturnType(), methodCallId, callFlags, callType);
            // 生成被调用方法信息（包含方法注解信息、方法调用业务功能数据）
            String calleeInfo = genCalleeInfo(methodCallLineData4Er);
            if (calleeInfo == null) {
                return false;
            }

            // 若当前被调用方法在调用方法中已被调用过则忽略
            if (recordedCalleeStack != null && checkIgnoreDupCalleeInOneCaller(recordedCalleeStack, callGraphNode4CallerStack, calleeInfo, methodCallId)) {
                continue;
            }

            // 无论方法调用链是否写文件，都需要添加
            methodCallLineData4ErList.add(methodCallLineData4Er);
            if (callGraphReturnInMemory) {
                // 方法调用链当前行的数据记录到用于返回的列表
                allMethodCallLineData4ErList.add(methodCallLineData4Er);
            }
            // 查询到记录，处理输出记录数
            int recordNum = recordNumCounter.addAndGet(1);
            showCalleeMethodNum = handleOutputLineNumber(recordNum, startCallerFullMethod);
            if (genCallGraphNumLimit > 0 && recordNum >= genCallGraphNumLimit) {
                // 处理记录数已达到限制的方法
                handleNumExceedMethod(startCallerFullMethod, recordNum, callGraphWriter, methodCallLineData4ErList);
                return true;
            }

            // 处理方法调用的节点信息
            Integer cycleCallLevel = handleCallerNodeInfo(callGraphNode4CallerStack, actualCalleeMethodHash, actualCalleeFullMethod, showCalleeMethodNum);
            methodCallLineData4Er.setCycleCallLevel(cycleCallLevel);

            if (callGraphJson != null) {
                // 记录方法调用链JSON格式
                recordCallGraphJson(callerFullMethod, methodCall.getCallerReturnType(), actualCalleeFullMethod, methodCall.getRawReturnType(), methodCall.getCallerLineNumber(),
                        methodCallLevel, callGraphJson);
            }

            // 记录可能出现一对多的方法调用
            if (!useNeo4j() && !recordMethodCallMayBeMulti(methodCallId, callType)) {
                return false;
            }

            // 更新当前处理节点的id
            callGraphNode4CallerStack.peek().setMethodCallId(methodCallId);

            if (cycleCallLevel != null) {
                // 出现循环调用，不再往下处理被调用的方法
                continue;
            }

            // 获取下一层节点
            CallGraphNode4Caller nextCallGraphNode4Caller = new CallGraphNode4Caller(actualCalleeMethodHash, JavaCG2Constants.METHOD_CALL_ID_MIN_BEFORE, actualCalleeFullMethod);
            callGraphNode4CallerStack.push(nextCallGraphNode4Caller);

            // 继续下一层处理
            if (ignoreDupCalleeInOneCaller) {
                // 开始处理下一层，设置对应的Set
                recordedCalleeStack.push(new HashSet<>());
            }
        }
    }

    /**
     * 处理输出记录数
     *
     * @param recordNum
     * @param startCallerFullMethod
     * @return true: 已达到指定的数量 false: 未达到指定的数量
     */
    private boolean handleOutputLineNumber(int recordNum, String startCallerFullMethod) {
        if (recordNum % JACGConstants.NOTICE_LINE_NUM == 0) {
            logger.info("记录数达到 {} {}", recordNum, startCallerFullMethod);
            return true;
        }
        return false;
    }

    // 处理子类方法调用父类方法的相关信息
    private MethodAndHash handleChildCallSuperInfo(ListAsStack<ChildCallSuperInfo> childCallSuperInfoStack,
                                                   int nodeLevel,
                                                   String calleeFullMethod,
                                                   String calleeMethodReturnType,
                                                   String callerFullMethod,
                                                   String callType,
                                                   String calleeMethodHash) {
        if (useNeo4j()) {
            return new MethodAndHash(calleeFullMethod, calleeMethodReturnType, calleeMethodHash);
        }

        if (JavaCG2CallTypeEnum.isChildCallSuperType(callType)) {
            // 当前方法调用类型是子类调用父类方法，记录子类方法调用父类方法对应信息的栈入栈
            String callerClassName = JavaCG2ClassMethodUtil.getClassNameFromMethod(callerFullMethod);
            String callerSimpleClassName = dbOperWrapper.querySimpleClassName(callerClassName);
            ChildCallSuperInfo childCallSuperInfo = new ChildCallSuperInfo(nodeLevel, callerSimpleClassName, callerClassName, callerFullMethod);
            childCallSuperInfoStack.push(childCallSuperInfo);
            return new MethodAndHash(calleeFullMethod, calleeMethodReturnType, calleeMethodHash);
        }

        // 获取子类的被调用方法
        Pair<Boolean, MethodAndHash> pair = getCCSChildFullMethod(childCallSuperInfoStack, calleeFullMethod, calleeMethodReturnType);
        if (Boolean.TRUE.equals(pair.getLeft())) {
            // 使用子类的被调用方法
            return pair.getRight();
        }

        return new MethodAndHash(calleeFullMethod, calleeMethodReturnType, calleeMethodHash);
    }

    /**
     * 获取子类的被调用方法，若不满足则使用原始方法
     *
     * @param childCallSuperInfoStack
     * @param calleeFullMethod
     * @return left true: 使用子类的被调用方法 false: 使用原始的被调用方法
     * @return right: 子类的被调用方法、方法HASH+长度
     */
    private Pair<Boolean, MethodAndHash> getCCSChildFullMethod(ListAsStack<ChildCallSuperInfo> childCallSuperInfoStack, String calleeFullMethod, String calleeMethodReturnType) {
        // 判断子类方法调用父类方法对应信息的栈是否有数据
        if (childCallSuperInfoStack.isEmpty()) {
            return new ImmutablePair<>(Boolean.FALSE, null);
        }

        String calleeMethodWithArgs = JACGClassMethodUtil.getMethodNameWithArgsFromFull(calleeFullMethod);
        if (calleeMethodWithArgs.startsWith(JavaCG2CommonNameConstants.METHOD_NAME_INIT)) {
            // 被调用方法为构造函数，使用原始被调用方法
            return new ImmutablePair<>(Boolean.FALSE, null);
        }

        String calleeClassName = JavaCG2ClassMethodUtil.getClassNameFromMethod(calleeFullMethod);
        String calleeSimpleClassName = dbOperWrapper.querySimpleClassName(calleeClassName);

        String ccsChildFullMethod = null;
        String ccsChildMethodHash = null;
        // 保存上一次处理的被调用唯一类名
        String lastChildCallerSimpleClassName = null;
        // 对子类方法调用父类方法对应信息的栈，从栈顶往下遍历
        for (int i = childCallSuperInfoStack.getHead(); i >= 0; i--) {
            ChildCallSuperInfo childCallSuperInfo = childCallSuperInfoStack.getElementAt(i);
            String childCallerSimpleClassName = childCallSuperInfo.getChildCallerSimpleClassName();

            if (lastChildCallerSimpleClassName != null) {
                if (!jacgExtendsImplHandler.checkExtendsOrImplBySimple(lastChildCallerSimpleClassName, childCallerSimpleClassName)) {
                    // 当前已不是第一次处理，判断上次的子类是否为当前子类的父类，若是则可以继续处理，若否则结束循环
                    break;
                }
                logger.debug("继续处理子类 {} {}", lastChildCallerSimpleClassName, childCallerSimpleClassName);
            }
            lastChildCallerSimpleClassName = childCallerSimpleClassName;

            // 判断子类方法调用父类方法对应信息的栈的调用类（对应子类）是否为当前被调用类的子类/实现类
            if (!jacgExtendsImplHandler.checkExtendsOrImplBySimple(calleeSimpleClassName, childCallerSimpleClassName)) {
                // 子类方法调用父类方法对应信息的栈的调用类（对应子类）不是当前被调用类的子类/实现类
                continue;
            }
            // 子类方法调用父类方法对应信息的栈的调用类为当前被调用类的子类
            String tmpCcsChildFullMethod = JavaCG2ClassMethodUtil.formatFullMethodWithArgTypes(childCallSuperInfo.getChildCallerClassName(), calleeMethodWithArgs);

            // 判断子类方法是否有被调用方法
            String tmpCcsChildMethodHash = dbOperWrapper.queryMethodHashByPrefix(childCallerSimpleClassName, tmpCcsChildFullMethod, calleeMethodReturnType);
            if (tmpCcsChildMethodHash == null) {
                // 子类方法不存在，需要继续使用栈中的数据进行处理，可能栈底是子类，栈顶是父类
                continue;
            }

            // 子类方法存在，使用子类方法
            ccsChildFullMethod = tmpCcsChildFullMethod;
            ccsChildMethodHash = tmpCcsChildMethodHash;
        }

        if (ccsChildFullMethod != null) {
            logger.debug("替换子类的向下的方法调用 {} {} {}", calleeFullMethod, ccsChildFullMethod, calleeMethodReturnType);
            // 使用子类对应的方法，返回子类方法及子类方法HASH+长度
            return new ImmutablePair<>(Boolean.TRUE, new MethodAndHash(ccsChildFullMethod, calleeMethodReturnType, ccsChildMethodHash));
        }
        // 使用原始被调用方法
        return new ImmutablePair<>(Boolean.FALSE, null);
    }

    /**
     * 处理被忽略的方法
     *
     * @param callType
     * @param calleeFullMethod
     * @param calleeMethodHash
     * @param callGraphNode4CallerStack
     * @param enabled
     * @param methodCallId
     * @param callFlags
     * @return true: 当前方法需要忽略 false: 当前方法不需要忽略
     */
    private boolean handleIgnoredMethod(String callType,
                                        String calleeFullMethod,
                                        String calleeMethodHash,
                                        ListAsStack<CallGraphNode4Caller> callGraphNode4CallerStack,
                                        int enabled,
                                        int methodCallId,
                                        int callFlags) {
        /*
            判断是否需要忽略
            - 调用方法与被调用方法HASH相同时忽略（bridge方法调用）
            - 调用方法需要忽略
            - 当前方法调用被禁用
            - 不显示dto的get/set方法
         */
        CallGraphNode4Caller callGraphNode4Caller = callGraphNode4CallerStack.peek();
        if (calleeMethodHash.equals(callGraphNode4Caller.getCallerMethodHash()) ||
                ignoreCurrentMethod(callType, calleeFullMethod) ||
                !JavaCG2YesNoEnum.isYes(enabled) ||
                (ignoreMethodTypeSet.contains(JACGMethodTypeEnum.MTE_DTO_GET_SET.getType()) && MethodCallFlagsEnum.MCFE_EE_DTO_GET_SET_METHOD.checkFlag(callFlags))) {
            // 更新当前处理节点的id
            callGraphNode4Caller.setMethodCallId(methodCallId);

            if (!JavaCG2YesNoEnum.isYes(enabled)) {
                // 记录被禁用的方法调用
                recordDisabledMethodCall(methodCallId, callType);
            }
            return true;
        }

        return false;
    }

    /**
     * 处理方法调用的节点信息
     * 检查是否出现循环调用
     * 打印当前所有被调用方法对应的被调用方法数
     *
     * @param callGraphNode4CallerStack
     * @param calleeMethodHash
     * @param calleeFullMethod
     * @param showCalleeMethodNum
     * @return null: 未出现循环调用，非null: 出现循环调用，值为发生循环调用的层级
     */
    private Integer handleCallerNodeInfo(ListAsStack<CallGraphNode4Caller> callGraphNode4CallerStack,
                                         String calleeMethodHash,
                                         String calleeFullMethod,
                                         boolean showCalleeMethodNum) {
        Integer cycleCallLevel = null;
        // 方法调用被调用方法数信息
        StringBuilder calleeMethodNumLogInfo = null;
        if (showCalleeMethodNum) {
            calleeMethodNumLogInfo = new StringBuilder();
        }

        // 循环调用的日志信息
        StringBuilder cycleCallLogInfo = new StringBuilder();

        // 遍历每个层级的被调用方法
        for (int i = 0; i <= callGraphNode4CallerStack.getHead(); i++) {
            CallGraphNode4Caller callGraphNode4Caller = callGraphNode4CallerStack.getElementAt(i);
            // 当前层级的被调用方法的被调用方法数加1
            callGraphNode4Caller.addCallerMethodNum();

            if (cycleCallLevel == null && calleeMethodHash.equals(callGraphNode4Caller.getCallerMethodHash())) {
                // 找到循环调用
                cycleCallLevel = i;
            }

            if (cycleCallLevel != null) {
                // 记录循环调用信息
                if (cycleCallLogInfo.length() > 0) {
                    cycleCallLogInfo.append("\n");
                }
                cycleCallLogInfo.append(JACGCallGraphFileUtil.genOutputLevelFlag(i))
                        .append(" ")
                        .append(callGraphNode4Caller.getCallerFullMethod());
            }

            if (showCalleeMethodNum) {
                // 记录方法调用被调用方法数
                if (calleeMethodNumLogInfo.length() > 0) {
                    calleeMethodNumLogInfo.append("\n");
                }
                calleeMethodNumLogInfo.append(JACGCallGraphFileUtil.genOutputLevelFlag(i))
                        .append(" 被调用方法数:").append(callGraphNode4Caller.getCalleeMethodNum()).append(" ")
                        .append(callGraphNode4Caller.getCallerFullMethod());
            }
        }

        // 每个层级的被调用方法遍历完之后的处理
        if (cycleCallLevel != null) {
            // 显示被循环调用的信息
            cycleCallLogInfo.append("\n")
                    .append(JACGCallGraphFileUtil.genCycleCallFlag(cycleCallLevel))
                    .append(" ")
                    .append(calleeFullMethod);
            logger.info("找到循环调用的方法\n{}", cycleCallLogInfo);
        }

        if (showCalleeMethodNum) {
            // 显示方法调用被调用方法数
            logger.info("被调用方法数（当前方法向下会调用的方法数量）\n{}", calleeMethodNumLogInfo);
        }
        return cycleCallLevel;
    }

    /**
     * 查询到被调用方法为空时的处理
     *
     * @param callGraphNode4CallerStack
     * @param childCallSuperInfoStack
     * @param recordedCalleeStack
     * @param calleeMethodNum
     * @param callGraphWriter
     * @param methodCallLineData4ErList
     * @return true: 需要结束循环 false: 不结束循环
     */
    private boolean handleCalleeEmptyResult(ListAsStack<CallGraphNode4Caller> callGraphNode4CallerStack, ListAsStack<ChildCallSuperInfo> childCallSuperInfoStack,
                                            ListAsStack<Set<String>> recordedCalleeStack, int calleeMethodNum, BufferedWriter callGraphWriter,
                                            List<MethodCallLineData4Er> methodCallLineData4ErList) throws IOException {
        if (calleeMethodNum == 0 && !methodCallLineData4ErList.isEmpty()) {
            // 当前方法向下没有调用其他方法
            StringBuilder callGraphInfo = callGraphWriteToFile ? new StringBuilder() : null;
            int calleeMethodListSize = methodCallLineData4ErList.size();
            for (int i = 0; i < calleeMethodListSize; i++) {
                MethodCallLineData4Er methodCallLineData4Er = methodCallLineData4ErList.get(i);
                if (i == calleeMethodListSize - 1) {
                    // 将最后一行记录添加向下没有方法调用标记
                    methodCallLineData4Er.setNoDownwardCallee(true);
                }
                // 生成方法调用链每行数据字符串
                String lineData = genMethodCallLineStr(methodCallLineData4Er);
                if (callGraphInfo != null) {
                    callGraphInfo.append(lineData).append(JavaCG2Constants.NEW_LINE);
                }
            }
            if (callGraphInfo != null) {
                // 将方法调用内容写入文件
                callGraphWriter.write(callGraphInfo.toString());
            }
            methodCallLineData4ErList.clear();
        }

        if (callGraphNode4CallerStack.atBottom()) {
            // 当前处理的节点为最上层节点，结束循环
            return true;
        }

        // 当前处理的节点不是最上层节点，返回上一层处理
        if (ignoreDupCalleeInOneCaller) {
            // 清空不再使用的下一层Set
            recordedCalleeStack.removeTop();
        }

        if (!childCallSuperInfoStack.isEmpty()) {
            // 记录子类方法调用父类方法对应信息的栈非空
            ChildCallSuperInfo topChildCallSuperInfo = childCallSuperInfoStack.peek();
            if (topChildCallSuperInfo.getChildCallerNodeLevel() == callGraphNode4CallerStack.getHead()) {
                // 记录子类方法调用父类方法对应信息的栈顶元素，与方法调用节点栈出栈的级别相同，出栈
                childCallSuperInfoStack.removeTop();
            }
        }

        // 删除栈顶元素
        callGraphNode4CallerStack.removeTop();

        return false;
    }

    // 生成被调用方法信息（包含方法注解信息、方法调用业务功能数据）
    private String genCalleeInfo(MethodCallLineData4Er methodCallLineData4Er) {
        MethodDetailNoReturnType actualMethodDetailNoReturnType = JACGClassMethodUtil.genMethodDetailNoReturnType(methodCallLineData4Er.getActualFullMethod());
        String calleeClassName = actualMethodDetailNoReturnType.getClassName();
        String calleeMethodName = actualMethodDetailNoReturnType.getMethodName();
        StringBuilder calleeInfo = new StringBuilder();
        if (OutputDetailEnum.ODE_0 == outputDetailEnum) {
            // # 0: 展示 完整类名+方法名+方法参数+返回类型
            calleeInfo.append(JavaCG2ClassMethodUtil.genFullMethodWithReturnType(methodCallLineData4Er.getActualFullMethod(), methodCallLineData4Er.getMethodReturnType()));
        } else if (OutputDetailEnum.ODE_1 == outputDetailEnum) {
            // # 1: 展示 完整类名+方法名+方法参数
            calleeInfo.append(methodCallLineData4Er.getActualFullMethod());
        } else if (OutputDetailEnum.ODE_2 == outputDetailEnum) {
            // # 2: 展示 完整类名+方法名
            calleeInfo.append(JACGClassMethodUtil.genClassAndMethodName(calleeClassName, calleeMethodName));
        } else {
            // # 3: 展示 简单类名（对于同名类展示完整类名）+方法名
            String calleeSimpleClassName = dbOperWrapper.querySimpleClassName(calleeClassName);
            calleeInfo.append(JACGClassMethodUtil.genClassAndMethodName(calleeSimpleClassName, calleeMethodName));
        }

        Integer callFlags = methodCallLineData4Er.getCallFlags();
        int methodCallId = methodCallLineData4Er.getMethodCallId();
        if (!useNeo4j()) {
            // 添加方法注解信息
            getMethodAnnotationInfo(calleeInfo, methodCallLineData4Er);

            // 添加方法调用业务功能数据，被调用方法HASH使用原始的
            if (!addBusinessData(methodCallId, callFlags, methodCallLineData4Er.getRawMethodHash(), calleeInfo, methodCallLineData4Er)) {
                return null;
            }

            // 为向下的方法完整调用链添加默认的方法调用业务功能数据
            if (!addDefaultBusinessData4er(callFlags, calleeClassName, calleeMethodName, calleeInfo, methodCallLineData4Er)) {
                return null;
            }
            String callType = methodCallLineData4Er.getCallType();
            // 为方法调用信息增加是否在其他线程执行标志
            addRunInOtherThread(calleeInfo, methodCallId, callType, methodCallLineData4Er);

            // 为方法调用信息增加是否在事务中执行标志
            addRunInSpringTransaction(calleeInfo, methodCallId, callType, methodCallLineData4Er);
        }
        String calleeInfoStr = calleeInfo.toString();
        methodCallLineData4Er.setCalleeInfo(calleeInfoStr);
        return calleeInfoStr;
    }

    /**
     * 若当前被调用方法在调用方法中已被调用过则忽略
     *
     * @param recordedCalleeStack
     * @param callGraphNode4CallerStack
     * @param calleeInfo
     * @param methodCallId
     * @return true: 需要忽略 false:不忽略
     */
    private boolean checkIgnoreDupCalleeInOneCaller(ListAsStack<Set<String>> recordedCalleeStack,
                                                    ListAsStack<CallGraphNode4Caller> callGraphNode4CallerStack,
                                                    String calleeInfo,
                                                    int methodCallId) {
        Set<String> callerRecordedCalleeSet = recordedCalleeStack.peek();
        if (!callerRecordedCalleeSet.add(calleeInfo)) {
            // 当前被调用方法在调用方法中已被调用过，忽略
            logger.debug("忽略一个方法中被调用多次的方法 {} {}", callGraphNode4CallerStack.getHead(), calleeInfo);

            // 更新当前处理节点的id
            callGraphNode4CallerStack.peek().setMethodCallId(methodCallId);
            return true;
        }
        return false;
    }

    // 记录方法调用链JSON格式
    private void recordCallGraphJson(String callerFullMethod, String callerReturnType, String calleeFullMethod, String calleeReturnType, int callerLineNumber,
                                     int methodCallLevel, CallGraphJson callGraphJson) {
        String callerMethodHash = JACGClassMethodUtil.genMethodHashWithLen(callerFullMethod, callerReturnType);
        String calleeMethodHash = JACGClassMethodUtil.genMethodHashWithLen(calleeFullMethod, calleeReturnType);
        CallGraphJsonMethodCall callGraphJsonMethodCall = new CallGraphJsonMethodCall(methodCallLevel, callerMethodHash, calleeMethodHash, callerLineNumber);

        MethodDetail callerMethodDetail = JACGClassMethodUtil.genMethodDetail(callerFullMethod, callerReturnType);
        CallGraphJsonMethod callerMethod = new CallGraphJsonMethod(callerMethodDetail);
        MethodDetail calleeMethodDetail = JACGClassMethodUtil.genMethodDetail(calleeFullMethod, calleeReturnType);
        CallGraphJsonMethod calleeMethod = new CallGraphJsonMethod(calleeMethodDetail);

        callGraphJson.addMethodCall(callGraphJsonMethodCall);
        callGraphJson.addMethod(callerMethod);
        callGraphJson.addMethod(calleeMethod);
    }

    // 获取调用方完整类名
    private String getCallerClassName(String callerSimpleClassName) {
        String existedClassName = simpleAndClassNameMap.get(callerSimpleClassName);
        if (existedClassName != null) {
            return existedClassName;
        }

        // 根据调用方简单类名，查找1个对应的完整方法
        String fullMethod = dbOperWrapper.queryOneFullMethodByCallerSCN(callerSimpleClassName);
        if (fullMethod == null) {
            logger.warn("从方法调用关系表未找到对应的完整类名 {}", callerSimpleClassName);
            return null;
        }

        String className = JavaCG2ClassMethodUtil.getClassNameFromMethod(fullMethod);
        simpleAndClassNameMap.putIfAbsent(callerSimpleClassName, className);
        return className;
    }

    // 为向下的方法完整调用链添加默认的方法调用业务功能数据
    private boolean addDefaultBusinessData4er(Integer callFlags,
                                              String calleeClassName,
                                              String calleeMethodName,
                                              StringBuilder callGraphInfo,
                                              MethodCallLineData4Er methodCallLineData4Er) {
        for (String businessDataType : businessDataTypeList) {
            DefaultBusinessDataTypeEnum defaultBusinessDataTypeEnum = DefaultBusinessDataTypeEnum.getFromType(businessDataType);
            switch (defaultBusinessDataTypeEnum) {
                case BDTE_MYBATIS_MYSQL_TABLE:
                    // 显示MyBatis的XML文件中对应的数据库表名
                    if (callFlags != null && !MethodCallFlagsEnum.MCFE_EE_MYBATIS_MAPPER.checkFlag(callFlags)) {
                        continue;
                    }

                    MyBatisMSTableInfo myBatisMySqlTableInfo = myBatisMSMapperEntityHandler.queryMyBatisMySqlTableInfo(calleeClassName, calleeMethodName);
                    if (myBatisMySqlTableInfo == null && callFlags != null) {
                        return false;
                    }
                    addBusinessData2CallGraphInfo(businessDataType, myBatisMySqlTableInfo, callGraphInfo, methodCallLineData4Er);
                    break;
                case BDTE_MYBATIS_MYSQL_WRITE_TABLE:
                    // 显示MyBatis的XML文件中对应的写数据库表名
                    if (callFlags != null && !MethodCallFlagsEnum.MCFE_EE_MYBATIS_MAPPER_WRITE.checkFlag(callFlags)) {
                        continue;
                    }

                    WriteDbData4MyBatisMSWriteTable writeDbData4MyBatisMSWriteTable = myBatisMSMapperEntityHandler.queryMyBatisMySqlWriteTableInfo(calleeClassName,
                            calleeMethodName);
                    if (writeDbData4MyBatisMSWriteTable == null && callFlags != null) {
                        return false;
                    }
                    addBusinessData2CallGraphInfo(businessDataType, writeDbData4MyBatisMSWriteTable, callGraphInfo, methodCallLineData4Er);
                    break;
                default:
                    break;
            }
        }
        return true;
    }

    // 生成方法调用链每行数据字符串，生成向下的方法调用链时使用
    @Override
    protected String genMethodCallLineStr(MethodCallLineData methodCallLineData) {
        MethodCallLineData4Er methodCallLineData4Er = (MethodCallLineData4Er) methodCallLineData;
        StringBuilder methodCallLineStr = new StringBuilder();
        // 生成输出文件前缀，包含了当前方法的调用层级
        String prefix = JACGCallGraphFileUtil.genOutputPrefix(methodCallLineData4Er.getMethodCallLevel());
        // 写入前缀：    "[2]#    "
        methodCallLineStr.append(prefix);

        if (methodCallLineData4Er.getMethodCallLevel() > JACGConstants.CALL_GRAPH_METHOD_LEVEL_START) {
            // 写入调用方行号信息，包括调用方代码行号：   "[Service1Impl:29]	"
            methodCallLineStr.append(JACGConstants.FLAG_LEFT_PARENTHESES).
                    append(methodCallLineData4Er.getCallerSimpleClassName()).
                    append(JavaCG2Constants.FLAG_COLON).
                    append(methodCallLineData4Er.getCallerLineNumber()).
                    append(JACGConstants.FLAG_RIGHT_PARENTHESES).
                    append(JavaCG2Constants.FLAG_TAB);
        }

        // 写入被调用方法信息
        methodCallLineStr.append(methodCallLineData4Er.getCalleeInfo());

        // 写入循环调用标志
        if (methodCallLineData4Er.getCycleCallLevel() != null) {
            methodCallLineStr.append(JavaCG2Constants.FLAG_TAB).append(JACGCallGraphFileUtil.genCycleCallFlag(methodCallLineData4Er.getCycleCallLevel()));
        }

        if (methodCallLineData4Er.isNoDownwardCallee()) {
            // 当前被调用方法向下没有被调用方法
            methodCallLineStr.append(JACGConstants.CALLEE_FLAG_NO_CALLEE);
        }

        return methodCallLineStr.toString();
    }

    /**
     * 获取内存中保存的所有方法调用链当前行的数据，包含起始方法的信息
     * 因此实际调用链的数量需要减一
     *
     * @return
     */
    public List<MethodCallLineData4Er> getAllMethodCallLineData4ErList() {
        if (!callGraphReturnInMemory) {
            throw new JavaCG2RuntimeException(String.format("仅当 %s=%s 时才允许使用当前方法", ConfigKeyEnum.CKE_CALL_GRAPH_RETURN_IN_MEMORY.getKey(), Boolean.TRUE));
        }
        return allMethodCallLineData4ErList;
    }
}
