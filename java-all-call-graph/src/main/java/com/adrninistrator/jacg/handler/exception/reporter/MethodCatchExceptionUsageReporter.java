package com.adrninistrator.jacg.handler.exception.reporter;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dto.method.ClassAndMethodName;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCatch;
import com.adrninistrator.jacg.handler.common.JACGReportConstants;
import com.adrninistrator.jacg.handler.dto.exception.BaseMethodCatchExceptionUsage;
import com.adrninistrator.jacg.handler.dto.exception.MCEU4MethodCallUseE;
import com.adrninistrator.jacg.handler.dto.exception.MCEU4ThrowE;
import com.adrninistrator.jacg.handler.exception.MethodExceptionHandler;
import com.adrninistrator.jacg.handler.reporter.AbstractReporter;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.writer.WriterSupportHeader;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.dto.counter.JavaCG2Counter;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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
 * @date 2024/1/3
 * @description: 生成方法中catch的异常对象使用情况的报告
 */
public class MethodCatchExceptionUsageReporter extends AbstractReporter {

    private static final Logger logger = LoggerFactory.getLogger(MethodCatchExceptionUsageReporter.class);

    public static final String METHOD_CATCH_EXCEPTION_USAGE_REPORT_ALL = "catch的异常对象使用情况-全部.md";
    public static final String METHOD_CATCH_EXCEPTION_USAGE_REPORT_CHECK = "catch的异常对象使用情况-需要检查.md";

    public static final String[] FILE_HEADER_ARRAY_ALL = new String[]{
            "编号",
            JACGReportConstants.COLUMN_FULL_METHOD,
            "try代码块起始行号",
            "try代码块结束行号",
            "catch代码块起始行号",
            "catch代码块结束行号",
            "catch的异常类型",
            "catch的异常变量名称",
            "catch的异常对象的使用方式描述",
            "catch的异常对象的使用详情"
    };

    public static final String[] FILE_HEADER_ARRAY_CHECK = new String[]{
            "编号",
            JACGReportConstants.COLUMN_FULL_METHOD,
            "try代码块起始行号",
            "try代码块结束行号",
            "catch代码块起始行号",
            "catch代码块结束行号",
            "catch的异常类型",
            "catch的异常对象的使用方式描述汇总"
    };

    public static final String FILE_HEADER_ALL = StringUtils.join(FILE_HEADER_ARRAY_ALL, JavaCG2Constants.FLAG_TAB);
    public static final String FILE_HEADER_CHECK = StringUtils.join(FILE_HEADER_ARRAY_CHECK, JavaCG2Constants.FLAG_TAB);

    // 方法catch信息序号
    private JavaCG2Counter catchSeq;

    public MethodCatchExceptionUsageReporter(ConfigureWrapper configureWrapper, String reportDirPath, boolean appendReportFile, boolean skipWriteDb) {
        super(configureWrapper, reportDirPath, appendReportFile, skipWriteDb);
    }

    /**
     * 选择需要使用的文件头-全部，可重载
     *
     * @return
     */
    protected String chooseFileHeader4All() {
        return FILE_HEADER_ALL;
    }

    /**
     * 选择需要使用的文件头-需要检查，可重载
     *
     * @return
     */
    protected String chooseFileHeader4Check() {
        return FILE_HEADER_CHECK;
    }

    /**
     * 在catch的异常对象使用情况报告文件中写入数据-全部，可重载
     *
     * @param writerSupportHeader
     * @param stringList
     * @throws IOException
     */
    protected void writeData4All(WriterSupportHeader writerSupportHeader, List<String> stringList) throws IOException {
        commonWriteData(writerSupportHeader, stringList);
    }

    /**
     * 在catch的异常对象使用情况报告文件中写入数据-需要检查，可重载
     *
     * @param writerSupportHeader
     * @param stringList
     * @throws IOException
     */
    protected void writeData4Check(WriterSupportHeader writerSupportHeader, List<String> stringList) throws IOException {
        commonWriteData(writerSupportHeader, stringList);
    }

    /**
     * 生成catch的异常对象信息的使用情况
     *
     * @param expectedMethods 预期会在参数中使用catch的异常对象的方法（例如在日志中打印异常堆栈）
     *                        格式：{类名}:{方法名}
     *                        示例：org.slf4j.Logger:error org.slf4j.Logger:info org.slf4j.Logger:warn
     * @return
     */
    public boolean genMethodCatchExceptionUsageReporter(String... expectedMethods) {
        if (ArrayUtils.isEmpty(expectedMethods)) {
            logger.error("预期会使用catch的异常对象的方法未在参数中指定");
            return false;
        }

        // 设置所有的类都需要处理（否则 throw new Exception() 不会被处理）
        configureWrapper.setAllowAllClasses();

        // 公共预处理，包含写数据库步骤
        if (!commonPreHandle()) {
            return false;
        }

        if (catchSeq == null) {
            catchSeq = new JavaCG2Counter(0);
        }

        // 将字符串形式的类名与方法名数组转换为对象列表形式
        List<ClassAndMethodName> expectedClassAndMethodNameList = JACGClassMethodUtil.genClassAndMethodNameListFromString(expectedMethods);
        String reportFilePathAll = genReportFilePath(METHOD_CATCH_EXCEPTION_USAGE_REPORT_ALL);
        String reportFilePathCheck = genReportFilePath(METHOD_CATCH_EXCEPTION_USAGE_REPORT_CHECK);
        try (MethodExceptionHandler methodExceptionHandler = new MethodExceptionHandler(configureWrapper);
             WriterSupportHeader writer4All = new WriterSupportHeader(reportFilePathAll, chooseFileHeader4All(), appendReportFile);
             WriterSupportHeader writer4Check = new WriterSupportHeader(reportFilePathCheck, chooseFileHeader4Check(), appendReportFile)) {
            // 查询方法catch信息表中所有的简单类名
            List<String> methodCatchSimpleClassNameList = methodExceptionHandler.queryMethodCatchSimpleClassNameList();
            if (JavaCG2Util.isCollectionEmpty(methodCatchSimpleClassNameList)) {
                logger.warn("未查询到方法catch信息");
                return true;
            }

            for (String methodCatchSimpleClassName : methodCatchSimpleClassNameList) {
                /*
                    记录已处理过的catch代码块开始指令偏移量，避免重复处理
                    key:    方法HASH+长度
                    value:  方法对应的已处理过的catch代码块开始指令偏移量集合
                 */
                Map<String, Set<Integer>> methodCatchStartOffsetMap = new HashMap<>();
                // 查询指定简单类名的方法catch信息
                List<WriteDbData4MethodCatch> methodCatchList = methodExceptionHandler.queryMethodCatchListBySimpleClassNameExcludeFlag(methodCatchSimpleClassName);
                for (WriteDbData4MethodCatch methodCatch : methodCatchList) {
                    // 处理一个方法catch
                    handleOneMethodCatch(methodCatch, methodCatchStartOffsetMap, methodExceptionHandler, writer4All, writer4Check, expectedClassAndMethodNameList);
                }
            }
            return true;
        } catch (Exception e) {
            logger.error("error ", e);
            return false;
        }
    }

    // 处理一个方法catch
    private void handleOneMethodCatch(WriteDbData4MethodCatch methodCatch, Map<String, Set<Integer>> methodCatchStartOffsetMap, MethodExceptionHandler methodExceptionHandler,
                                      WriterSupportHeader writer4All, WriterSupportHeader writer4Check, List<ClassAndMethodName> expectedClassAndMethodNameList) throws IOException {
        // 记录当前的方法catch是否按预期使用
        boolean expectedUsage = false;
        Set<Integer> methodCatchStartOffset = methodCatchStartOffsetMap.computeIfAbsent(methodCatch.getMethodHash(), k -> new HashSet<>());
        if (!methodCatchStartOffset.add(methodCatch.getCatchStartOffset())) {
            // 当前catch代码块开始指令偏移量已处理过时不再处理
            return;
        }
        int catchSeqValue = catchSeq.addAndGet();
        // 记录catch的异常对象使用方式描述
        Set<String> catchExceptionUsageDescriptionSet = new HashSet<>();

        // 查询指定的方法catch的异常对象信息的使用情况
        List<BaseMethodCatchExceptionUsage> methodCatchExceptionUsageList = methodExceptionHandler.queryMethodCatchExceptionUsage(methodCatch, expectedClassAndMethodNameList);
        for (BaseMethodCatchExceptionUsage methodCatchExceptionUsage : methodCatchExceptionUsageList) {
            if (methodCatchExceptionUsage.getClass().equals(MCEU4MethodCallUseE.class)) {
                // 方法调用的被调用对象或参数使用catch的异常对象
                MCEU4MethodCallUseE mceu4MethodCallUseE = (MCEU4MethodCallUseE) methodCatchExceptionUsage;
                // 检查catch代码块中被调用的方法是否属于预期的方法
                if (mceu4MethodCallUseE.isUseEInExpectedMethodCall()) {
                    expectedUsage = true;
                }
            } else if (methodCatchExceptionUsage.getClass().equals(MCEU4ThrowE.class)) {
                // 通过throw抛出catch的异常对象
                expectedUsage = true;
            }

            // 记录catch的异常对象使用方式描述
            catchExceptionUsageDescriptionSet.add(methodCatchExceptionUsage.getUsageDescription());
            List<String> stringList4All = new ArrayList<>();
            stringList4All.add(String.valueOf(catchSeqValue));
            stringList4All.add(methodCatch.getFullMethod());
            stringList4All.add(String.valueOf(methodCatch.getTryStartLineNumber()));
            stringList4All.add(String.valueOf(methodCatch.getTryEndLineNumber()));
            stringList4All.add(String.valueOf(methodCatch.getCatchStartLineNumber()));
            stringList4All.add(String.valueOf(methodCatch.getCatchEndLineNumber()));
            stringList4All.add(methodCatch.getCatchExceptionType());
            stringList4All.add(methodCatchExceptionUsage.getCatchExceptionVariableName());
            stringList4All.add(methodCatchExceptionUsage.getUsageDescription());
            stringList4All.add(methodCatchExceptionUsage.getUsageDetail());
            // 在catch的异常对象使用情况报告文件中写入数据-全部
            writeData4All(writer4All, stringList4All);
        }

        if (!expectedUsage) {
            // 当前catch的异常对象没有按预期使用
            List<String> stringList4Check = new ArrayList<>();
            stringList4Check.add(String.valueOf(catchSeqValue));
            stringList4Check.add(methodCatch.getFullMethod());
            stringList4Check.add(String.valueOf(methodCatch.getTryStartLineNumber()));
            stringList4Check.add(String.valueOf(methodCatch.getTryEndLineNumber()));
            stringList4Check.add(String.valueOf(methodCatch.getCatchStartLineNumber()));
            stringList4Check.add(String.valueOf(methodCatch.getCatchEndLineNumber()));
            stringList4Check.add(methodCatch.getCatchExceptionType());
            List<String> catchExceptionUsageDescriptionList = new ArrayList<>(catchExceptionUsageDescriptionSet);
            Collections.sort(catchExceptionUsageDescriptionList);
            stringList4Check.add(StringUtils.join(catchExceptionUsageDescriptionList, " "));
            writeData4Check(writer4Check, stringList4Check);
        }
    }

    public void setCatchSeq(JavaCG2Counter catchSeq) {
        this.catchSeq = catchSeq;
    }
}
