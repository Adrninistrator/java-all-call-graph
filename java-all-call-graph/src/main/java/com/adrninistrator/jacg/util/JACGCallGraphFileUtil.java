package com.adrninistrator.jacg.util;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.dto.call_line.CallGraphLineParsed;
import com.adrninistrator.jacg.dto.method.MethodDetail;
import com.adrninistrator.jacg.dto.method.MethodInfoInFileName;
import com.adrninistrator.jacg.extensions.dto.business_data.BaseBusinessData;
import com.adrninistrator.jacg.markdown.JACGMarkdownConstants;
import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.exceptions.JavaCGRuntimeException;
import com.adrninistrator.javacg.util.JavaCGUtil;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * @author adrninistrator
 * @date 2022/8/24
 * @description: 用于处理生成的调用链文件的工具类
 */
public class JACGCallGraphFileUtil {
    private static final Logger logger = LoggerFactory.getLogger(JACGCallGraphFileUtil.class);

    // 方法完整调用链文件中，代表方法调用第0层级的标志
    private static final String CALL_FLAG_LEVEL_0 = JACGConstants.FLAG_LEFT_PARENTHESES + "0" + JACGConstants.FLAG_RIGHT_PARENTHESES;

    private static final Map<Integer, String> OUTPUT_FLAG_MAP = new ConcurrentHashMap<>();

    /**
     * 判断调用链搜索结果文件中指定行是否为序号对应的行，以#开头
     *
     * @param line 文件行内容
     * @return
     */
    public static boolean isDataSeqLine(String line) {
        return StringUtils.startsWith(line, JACGMarkdownConstants.FLAG_TITLE) && StringUtils.contains(line, JACGConstants.FLAG_MD_LINE_NUMBER);
    }

    /**
     * 调用链搜索结果文件中序号对应行的序号值
     *
     * @param line 文件行内容
     * @return
     */
    public static int getDataSeqFromLine(String line) {
        String dataSeq = StringUtils.substringBetween(line, JACGMarkdownConstants.FLAG_SPACE, JACGMarkdownConstants.FLAG_DOT);
        if (!JavaCGUtil.isNumStr(dataSeq)) {
            throw new JavaCGRuntimeException("方法调用行内容非法 " + line);
        }
        return Integer.parseInt(dataSeq);
    }

    /**
     * 判断方法完整调用链文件中指定行是否为调用链对应的行
     * （或者是判断调用堆栈文件中指定行是否为调用堆栈对应的行）
     *
     * @param line
     * @return
     */
    public static boolean isCallGraphLine(String line) {
        return StringUtils.startsWith(line, JACGConstants.FLAG_LEFT_PARENTHESES);
    }

    /**
     * 判断是否为markdown的代码行
     *
     * @param line
     * @return
     */
    public static boolean isMarkdownCodeLine(String line) {
        return StringUtils.startsWith(line, JACGMarkdownConstants.FLAG_CODE);
    }

    /**
     * 生成方法对应的调用链文件名
     * 格式： [完整或简单类名]@[方法名]@[方法HASH+长度]
     *
     * @param simpleClassName 完整或简单类名
     * @param methodName      方法名
     * @param methodHash      方法HASH+长度
     * @return
     */
    public static String getCallGraphMethodFileName(String simpleClassName, String methodName, String methodHash) {
        return simpleClassName + JACGConstants.FLAG_AT +
                JACGClassMethodUtil.getSafeMethodName(methodName) + JACGConstants.FLAG_AT +
                methodHash;
    }

    /**
     * 生成内容为空的调用链文件名
     * 格式： [完整或简单类名]@[方法名]@[方法HASH+长度]
     *
     * @param simpleClassName 完整或简单类名
     * @param methodName      方法名
     * @return
     */
    public static String getEmptyCallGraphFileName(String simpleClassName, String methodName) {
        return simpleClassName + JACGConstants.FLAG_AT + JACGClassMethodUtil.getSafeMethodName(methodName) + JACGConstants.EXT_EMPTY_TXT;
    }

    /**
     * 判断文件中是否代表空的调用链文件
     *
     * @param fileName 文件名
     * @return
     */
    public static boolean isEmptyCallGraphFileName(String fileName) {
        return StringUtils.endsWith(fileName, JACGConstants.EXT_EMPTY_MD);
    }

    /**
     * 从方法对应的调用链文件路径，获取方法相关的信息
     * 文件名格式见以上getCallGraphMethodFileName方法
     *
     * @param fileName
     * @return
     */
    public static MethodInfoInFileName getMethodInfoFromFileName(String fileName) {
        if (fileName == null) {
            return null;
        }

        String fileNameWithOutExt = JACGFileUtil.getFileNameWithOutExt(fileName);
        // 以下文件名的格式，见RunnerGenAllGraph4Caller.handleOneTask()方法
        String[] array = StringUtils.splitPreserveAllTokens(fileNameWithOutExt, JACGConstants.FLAG_AT);
        if (array.length < JACGConstants.CALLER_FILE_NAME_SPLIT_BY_AT_MIN_COLUMNS) {
            // 因为文件名中可能包括代码行号，会多一个使用@分隔的数据，因此分隔后的列长度可能会更多，但不能更少
            logger.error("文件名使用{}分隔后，列数小于 {} {}", JACGConstants.FLAG_AT, JACGConstants.CALLER_FILE_NAME_SPLIT_BY_AT_MIN_COLUMNS, fileName);
            return null;
        }

        return new MethodInfoInFileName(array[0], JACGClassMethodUtil.recoveryMethodName(array[1]), array[2]);
    }

    /**
     * 从方法对应的调用链文件名中获取对应的类名
     *
     * @param fileName
     * @return
     */
    public static String getClassNameFromMethodFileName(String fileName) {
        if (fileName == null) {
            return null;
        }
        // 文件名使用@分隔后，类名在第1列
        return StringUtils.substringBefore(fileName, JACGConstants.FLAG_AT);
    }

    /**
     * 生成结果文件中的级别空格标志
     * 按照级别数量增加空格
     *
     * @param level
     * @return
     */
    public static String genOutputLevelSpaceFlag(int level) {
        if (level < 0) {
            throw new JavaCGRuntimeException("指定的方法级别非法 " + level);
        }
        if (level == 0) {
            return "";
        }
        return OUTPUT_FLAG_MAP.computeIfAbsent(level, k -> StringUtils.repeat(JACGConstants.OUTPUT_SPLIT_FLAG, k));
    }

    /**
     * 生成结果文件中的级别标志
     *
     * @param level
     * @return
     */
    public static String genOutputLevelFlag(int level) {
        return JACGConstants.FLAG_LEFT_PARENTHESES + level + JACGConstants.FLAG_RIGHT_PARENTHESES;
    }

    /**
     * 生成输出文件前缀，包含了当前方法的调用层级
     *
     * @param level
     * @return
     */
    public static String genOutputPrefix(int level) {
        return genOutputLevelFlag(level) + JavaCGConstants.FLAG_HASHTAG + genOutputLevelSpaceFlag(level);
    }

    /**
     * 生成循环调用标志
     *
     * @param back2Level
     * @return
     */
    public static String genCycleCallFlag(int back2Level) {
        return String.format(JACGConstants.CALL_FLAG_CYCLE, back2Level);
    }

    /**
     * 替换TAB、回车、换行等字符
     * 假如调用链文件数据中包含了以上字符，会导致调用链文件行分隔时出现问题，因此需要替换
     *
     * @param data
     * @return
     */
    public static String replaceSplitChars(String data) {
        if (data == null) {
            return "";
        }
        return data.replace("\t", "")
                .replace("\r", "")
                .replace("\n", "");
    }

    /**
     * 对方法完整调用链文件行进行分隔
     *
     * @param line
     * @param calleeGraph true: 向上的方法完整调用链文件 false: 向下的方法完整调用链文件
     * @return
     */
    public static String[] splitCallGraphLine(String line, boolean calleeGraph) {
        if (StringUtils.isBlank(line)) {
            throw new JavaCGRuntimeException("传入的行内容为空");
        }

        String[] array = StringUtils.splitPreserveAllTokens(line, JACGConstants.FLAG_TAB);
        if (calleeGraph) {
            if (array.length < JACGConstants.CALL_GRAPH_EE_LINE_MIN_COLUMN_NUM) {
                throw new JavaCGRuntimeException("向上的方法完整调用链文件分隔后列数太小 " + array.length);
            }
        } else if (array.length < JACGConstants.CALL_GRAPH_ER_LINE_MIN_COLUMN_NUM) {
            throw new JavaCGRuntimeException("向下的方法完整调用链文件分隔后列数太小 " + array.length);
        }
        return array;
    }

    /**
     * 获取方法级别为0的完整方法及注解
     *
     * @param column1
     * @return
     */
    private static String getFullMethodWithAnnotations4Level0(String column1) {
        return StringUtils.substringAfter(column1, JavaCGConstants.FLAG_HASHTAG);
    }

    /**
     * 为向上的方法完整调用链解析每行包含的内容
     *
     * @param line
     * @return
     */
    public static CallGraphLineParsed parseCallGraphLine4ee(String line) {
        if (line == null) {
            return null;
        }

        // 对方法完整调用链文件行进行分隔
        String[] array = splitCallGraphLine(line, true);
        String column1 = array[0];
        String fullMethodWithAnnotations;
        int nextStartIndex;

        // 获取方法级别
        int methodLevel = getMethodLevel(line);
        if (methodLevel == JACGConstants.CALL_GRAPH_METHOD_LEVEL_START) {
            // 获取方法级别为0的完整方法及注解
            fullMethodWithAnnotations = getFullMethodWithAnnotations4Level0(column1);
            nextStartIndex = 1;
        } else {
            // 方法级别大于0
            // 获取第1个#之后的内容
            int indexHashTag = column1.indexOf(JavaCGConstants.FLAG_HASHTAG);
            if (indexHashTag == -1) {
                throw new JavaCGRuntimeException(line + " 未找到字符 " + JavaCGConstants.FLAG_HASHTAG);
            }
            String column1SubString = column1.substring(indexHashTag + JavaCGConstants.FLAG_HASHTAG.length());
            // 获取第1个非空格开始的子字符串
            fullMethodWithAnnotations = JACGUtil.getFirstExcludeSubString(column1SubString, JACGConstants.FLAG_CHAR_SPACE);
            nextStartIndex = 2;
        }
        // 为方法完整调用链解析每行包含的内容
        return parseCallGraphLine(line, methodLevel, fullMethodWithAnnotations, array, nextStartIndex);
    }

    /**
     * 为向下的方法完整调用链解析每行包含的内容
     *
     * @param line
     * @return
     */
    public static CallGraphLineParsed parseCallGraphLine4er(String line) {
        if (line == null) {
            return null;
        }

        // 对方法完整调用链文件行进行分隔
        String[] array = splitCallGraphLine(line, false);
        String fullMethodWithAnnotations;
        int nextStartIndex;

        // 获取方法级别
        int methodLevel = getMethodLevel(line);
        if (methodLevel == JACGConstants.CALL_GRAPH_METHOD_LEVEL_START) {
            // 获取方法级别为0的完整方法及注解
            fullMethodWithAnnotations = getFullMethodWithAnnotations4Level0(array[0]);
            nextStartIndex = 1;
        } else {
            // 方法级别大于0
            fullMethodWithAnnotations = array[1];
            nextStartIndex = 2;
        }
        // 为方法完整调用链解析每行包含的内容
        return parseCallGraphLine(line, methodLevel, fullMethodWithAnnotations, array, nextStartIndex);
    }

    /**
     * 获取方法级别
     *
     * @param line
     * @return
     */
    public static int getMethodLevel(String line) {
        String level = StringUtils.substringBetween(line, JACGConstants.FLAG_LEFT_PARENTHESES, JACGConstants.FLAG_RIGHT_PARENTHESES);
        if (!JavaCGUtil.isNumStr(level)) {
            throw new JavaCGRuntimeException("方法调用行内容非法 " + line);
        }
        return Integer.parseInt(level);
    }

    /**
     * 为方法完整调用链解析每行包含的内容
     *
     * @param line                      行内容
     * @param methodLevel               方法级别
     * @param fullMethodWithAnnotations 完整方法及注解
     * @param lineColumns               行内容分隔后的列
     * @param nextStartIndex            后续内容起始下标
     * @return
     */
    private static CallGraphLineParsed parseCallGraphLine(String line,
                                                          int methodLevel,
                                                          String fullMethodWithAnnotations,
                                                          String[] lineColumns,
                                                          int nextStartIndex) {
        if (fullMethodWithAnnotations == null) {
            throw new JavaCGRuntimeException("获取方法与注解信息失败 " + line);
        }

        CallGraphLineParsed callGraphLineParsed = new CallGraphLineParsed();
        callGraphLineParsed.setMethodLevel(methodLevel);

        // 处理完整方法及注解
        handleFullMethodWithAnnotations(callGraphLineParsed, fullMethodWithAnnotations);

        if (lineColumns.length <= nextStartIndex) {
            // 当前行不存在需要继续处理的列
            return callGraphLineParsed;
        }

        // 当前行存在需要继续处理的列
        List<BaseBusinessData> businessDataList = new ArrayList<>();
        for (int i = nextStartIndex; i < lineColumns.length; i++) {
            String column = lineColumns[i];
            if (column.startsWith(JACGConstants.CALL_FLAG_BUSINESS_DATA)) {
                // 方法调用业务功能数据
                String businessDataStr = StringUtils.substringAfter(column, JACGConstants.CALL_FLAG_BUSINESS_DATA);
                // @之前为类型，@之后为值
                String businessDataType = StringUtils.substringBefore(businessDataStr, JACGConstants.FLAG_AT);
                String businessDataValue = StringUtils.substringAfter(businessDataStr, JACGConstants.FLAG_AT);
                // 以下分别为方法调用业务功能数据的类型与值
                BaseBusinessData businessData = new BaseBusinessData(businessDataType, businessDataValue);
                businessDataList.add(businessData);
            } else if (column.startsWith(JACGConstants.CALL_FLAG_CYCLE_START) && column.endsWith(JACGConstants.CALL_FLAG_CYCLE_END)) {
                // 出现循环调用
                callGraphLineParsed.setCycleCall(true);
                String level = StringUtils.substringBetween(column, JACGConstants.FLAG_LEFT_PARENTHESES, JACGConstants.FLAG_RIGHT_PARENTHESES);
                if (level == null) {
                    throw new JavaCGRuntimeException("方法调用行内容非法 " + line);
                }
                callGraphLineParsed.setCycleCallLevel(Integer.parseInt(level));
            } else if (JACGConstants.CALLEE_FLAG_ENTRY_NO_TAB.equals(column)) {
                // 入口方法
                callGraphLineParsed.setEntryMethod(true);
            } else if (JACGConstants.CALL_FLAG_RUN_IN_OTHER_THREAD_NO_TAB.equals(column)) {
                // 在其他线程中执行
                callGraphLineParsed.setRunInOtherThread(true);
            } else if (JACGConstants.CALL_FLAG_RUN_IN_TRANSACTION_NO_TAB.equals(column)) {
                // 在事务中执行
                callGraphLineParsed.setRunInTransaction(true);
            }
        }
        callGraphLineParsed.setBusinessDataList(businessDataList);
        return callGraphLineParsed;
    }

    /**
     * 判断是否包含在其他线程中执行的标记（不对行数据进行完整解析）
     *
     * @param line
     * @return
     */
    public static boolean checkRunInOtherThread(String line) {
        return StringUtils.contains(line, JACGConstants.CALL_FLAG_RUN_IN_OTHER_THREAD);
    }

    /**
     * 判断是否包含在事务中执行的标记（不对行数据进行完整解析）
     *
     * @param line
     * @return
     */
    public static boolean checkRunInTransaction(String line) {
        return StringUtils.contains(line, JACGConstants.CALL_FLAG_RUN_IN_TRANSACTION);
    }

    // 处理完整方法及注解
    private static void handleFullMethodWithAnnotations(CallGraphLineParsed callGraphLineParsed, String fullMethodWithAnnotations) {
        int index = fullMethodWithAnnotations.indexOf(JACGConstants.FLAG_AT);
        String fullMethod;
        if (index == -1) {
            // 方法上不存在注解
            fullMethod = fullMethodWithAnnotations;
        } else {
            // 方法上存在注解
            fullMethod = fullMethodWithAnnotations.substring(0, index);
            String annotations = fullMethodWithAnnotations.substring(index + JACGConstants.FLAG_AT.length());
            String[] annotationArray = StringUtils.splitPreserveAllTokens(annotations, JACGConstants.FLAG_AT);
            callGraphLineParsed.setAnnotations(annotationArray);
        }
        MethodDetail methodDetail = JACGClassMethodUtil.genMethodDetail(fullMethod);
        callGraphLineParsed.setMethodDetail(methodDetail);
    }

    /**
     * 判断指定的方法调用层级是否为第0级
     *
     * @param line
     * @return
     */
    public static boolean isCallGraphLevel0(String line) {
        return StringUtils.startsWith(line, CALL_FLAG_LEVEL_0);
    }

    private JACGCallGraphFileUtil() {
        throw new IllegalStateException("illegal");
    }
}
