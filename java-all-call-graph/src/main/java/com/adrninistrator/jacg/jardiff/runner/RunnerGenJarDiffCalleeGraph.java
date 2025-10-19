package com.adrninistrator.jacg.jardiff.runner;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.OutputDetailEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.conf.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.dto.calleemethodinfo.BaseCalleeMethodInfo;
import com.adrninistrator.jacg.dto.callstack.CallStackFileResult;
import com.adrninistrator.jacg.dto.callstack.CalleeStackSummary;
import com.adrninistrator.jacg.dto.entrymethodinfo.BaseEntryMethodInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4JarInfo;
import com.adrninistrator.jacg.findstack.FindCallStackTrace;
import com.adrninistrator.jacg.handler.calleemethodinfo.AbstractCalleeMethodInfoFiller;
import com.adrninistrator.jacg.handler.entrymethodinfo.AbstractEntryMethodInfoFiller;
import com.adrninistrator.jacg.util.JACGCallStackUtil;
import com.adrninistrator.jacg.util.JACGJsonUtil;
import com.adrninistrator.jacg.writer.WriterSupportHeader;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.conf.JavaCG2ConfigureWrapper;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;
import com.adrninistrator.javacg2.util.JavaCG2FileUtil;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2024/3/9
 * @description: 比较新旧两个目录中不同版本jar文件的方法修改情况，以及发生变化的方法的影响范围（生成向上的完整方法调用链及调用堆栈）
 */
public class RunnerGenJarDiffCalleeGraph extends AbstractRunnerGenJarDiffCallGraph {

    private static final Logger logger = LoggerFactory.getLogger(RunnerGenJarDiffCalleeGraph.class);

    public static final String[] FILE_HEADER_ARRAY_MODIFIED_METHODS_STACK = new String[]{
            "新jar文件名称",
            "被调用完整方法",
            "调用堆栈在文件中的序号",
            "上层调用完整方法",
            "入口方法",
            "被调用方法信息",
            "入口方法信息",
    };

    public static final String FILE_HEADER_MODIFIED_METHODS_STACK = StringUtils.join(FILE_HEADER_ARRAY_MODIFIED_METHODS_STACK, JavaCG2Constants.FLAG_TAB);

    // 对入口方法信息进行填充的类
    private final AbstractEntryMethodInfoFiller[] entryMethodInfoFillers;

    // 对被调用方法信息进行填充的类
    private final AbstractCalleeMethodInfoFiller[] calleeMethodInfoFillers;

    public RunnerGenJarDiffCalleeGraph() {
        this(new JavaCG2ConfigureWrapper(false), new ConfigureWrapper(false), null, null);
    }

    public RunnerGenJarDiffCalleeGraph(JavaCG2ConfigureWrapper javaCG2ConfigureWrapper, ConfigureWrapper configureWrapper,
                                       AbstractCalleeMethodInfoFiller[] calleeMethodInfoFillers, AbstractEntryMethodInfoFiller[] entryMethodInfoFillers) {
        super(javaCG2ConfigureWrapper, configureWrapper);
        this.calleeMethodInfoFillers = calleeMethodInfoFillers;
        this.entryMethodInfoFillers = entryMethodInfoFillers;
    }

    // 处理发生变化的jar文件与方法
    @Override
    protected boolean handleModifiedJarAndMethods(ConfigureWrapper configureWrapperNew, Map<String, WriteDbData4JarInfo> modifiedJarMap, Map<String, String> modifiedClassJarMap,
                                                  Set<String> modifiedMethodSet, List<String> jarFileNameListNew, String genAllCallGraphDir) {
        configureWrapperNew.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_OUTPUT_DETAIL, OutputDetailEnum.ODE_0.getDetail());
        configureWrapperNew.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_GEN_STACK_OTHER_FORMS, Boolean.TRUE.toString());
        configureWrapperNew.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLEE, modifiedMethodSet);
        configureWrapperNew.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_FIND_STACK_KEYWORD_4EE, JACGConstants.CALLEE_FLAG_ENTRY);
        FindCallStackTrace findCallStackTrace = new FindCallStackTrace(true, configureWrapperNew);
        // 设置生成完整方法调用链目录
        findCallStackTrace.setCurrentOutputDirPath(genAllCallGraphDir);
        // 生成发生变化的方法到入口方法的调用堆栈文件
        CallStackFileResult callStackFileResult = findCallStackTrace.find();
        if (!callStackFileResult.isSuccess()) {
            return false;
        }

        List<String> otherFormsStackDirPathList = callStackFileResult.getOtherFormsStackDirPathList();
        if (JavaCG2Util.isCollectionEmpty(otherFormsStackDirPathList)) {
            logger.warn("生成发生变化的方法到入口方法的其他形式的调用堆栈文件为空");
            return true;
        }
        // 生成jar文件中发生变化的方法基本信息，当前的输出目录作为输出以下文件的目录
        if (!writeModifiedMethodsBaseFile(currentOutputDirPath, jarFileNameListNew, modifiedJarMap)) {
            return false;
        }

        String modifiedMethodsStackFilePath = currentOutputDirPath + File.separator + JACGConstants.FILE_JAR_DIFF_MODIFIED_METHODS_STACK;
        try (WriterSupportHeader modifiedMethodsStackWriter = new WriterSupportHeader(modifiedMethodsStackFilePath, FILE_HEADER_MODIFIED_METHODS_STACK)) {
            // 生成jar文件中发生变化的方法的调用堆栈信息
            for (String otherFormsStackDirPath : otherFormsStackDirPathList) {
                // 处理其他形式的调用堆栈文件目录
                handleOtherFormsStackDir(modifiedMethodsStackWriter, otherFormsStackDirPath, modifiedClassJarMap);
            }
        } catch (Exception e) {
            logger.error("error ", e);
            return false;
        }
        return textFileToExcel(modifiedMethodsStackFilePath);
    }

    @Override
    protected OtherConfigFileUseListEnum chooseCompareDirEnum() {
        return OtherConfigFileUseListEnum.OCFULE_JAR_DIFF_CALLEE_GRAPH_DIR;
    }

    // 处理其他形式的调用堆栈文件目录
    private void handleOtherFormsStackDir(WriterSupportHeader modifiedMethodsStackWriter, String otherFormsStackDirPath, Map<String, String> modifiedClassJarMap) throws IOException {
        String summaryFilePath = otherFormsStackDirPath + File.separator + JACGConstants.FILE_CALLEE_STACK_SUMMARY;
        try (BufferedReader reader = JavaCG2FileUtil.genBufferedReader(summaryFilePath)) {
            String line;
            int lineNum = 0;
            while ((line = reader.readLine()) != null) {
                lineNum++;
                if (lineNum == 1) {
                    // 跳过首行表头
                    continue;
                }
                CalleeStackSummary calleeStackSummary = JACGCallStackUtil.parseCalleeStackSummaryFromLine(line);
                String calleeClassName = JavaCG2ClassMethodUtil.getClassNameFromMethod(calleeStackSummary.getCalleeMethod());
                String jarName = modifiedClassJarMap.get(calleeClassName);

                // 查询被调用方法的信息
                String calleeMethodInfo = queryCalleeMethodInfo(calleeStackSummary.getCalleeMethod());
                // 查询入口方法的信息
                String entryMethodInfo = queryEntryMethodInfo(calleeStackSummary.getKeywordMethod(), calleeStackSummary.getKeywordMethodReturnType());

                modifiedMethodsStackWriter.writeDataInLine(jarName, calleeStackSummary.getCalleeMethod(), calleeStackSummary.getStackSeq(),
                        calleeStackSummary.getUpwardCallerMethod(), calleeStackSummary.getKeywordMethod(), calleeMethodInfo, entryMethodInfo);
            }
        }
    }

    // 查询入口方法的信息
    private String queryEntryMethodInfo(String entryMethod, String entryMethodReturnType) {
        if (ArrayUtils.isEmpty(entryMethodInfoFillers)) {
            return "";
        }

        // 使用指定的入口方法信息填充类进行处理
        for (AbstractEntryMethodInfoFiller entryMethodInfoFiller : entryMethodInfoFillers) {
            BaseEntryMethodInfo entryMethodInfo = entryMethodInfoFiller.query(entryMethod, entryMethodReturnType);
            if (entryMethodInfo != null) {
                return JACGJsonUtil.getJsonStr(entryMethodInfo);
            }
        }
        return "";
    }

    // 查询被调用方法的信息
    private String queryCalleeMethodInfo(String calleeFullMethod) {
        if (ArrayUtils.isEmpty(calleeMethodInfoFillers)) {
            return "";
        }

        // 使用指定的入口方法信息填充类进行处理
        for (AbstractCalleeMethodInfoFiller calleeMethodInfoFiller : calleeMethodInfoFillers) {
            BaseCalleeMethodInfo calleeMethodInfo = calleeMethodInfoFiller.query(calleeFullMethod);
            if (calleeMethodInfo != null) {
                return JACGJsonUtil.getJsonStr(calleeMethodInfo);
            }
        }
        return "";
    }
}
