package com.adrninistrator.jacg.util;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.dto.callline.CallGraphLineParsed;
import com.adrninistrator.jacg.dto.method.MethodDetail;
import com.adrninistrator.jacg.handler.dto.businessdata.BaseBusinessData;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;
import com.adrninistrator.javacg2.markdown.MarkdownConstants;
import com.adrninistrator.javacg2.util.JavaCG2Util;
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
        return StringUtils.startsWith(line, MarkdownConstants.FLAG_TITLE) && StringUtils.contains(line, JACGConstants.FLAG_MD_LINE_NUMBER);
    }

    /**
     * 调用链搜索结果文件中序号对应行的序号值
     *
     * @param line 文件行内容
     * @return
     */
    public static int getDataSeqFromLine(String line) {
        String dataSeq = StringUtils.substringBetween(line, MarkdownConstants.FLAG_SPACE, MarkdownConstants.FLAG_DOT);
        if (!JavaCG2Util.isNumStr(dataSeq)) {
            throw new JavaCG2RuntimeException("方法调用行内容非法 " + line);
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
        return StringUtils.startsWith(line, MarkdownConstants.FLAG_CODE);
    }

    /**
     * 生成方法对应的调用链文件名
     * 格式： {完整或简单类名}@{方法名}@{方法HASH+长度}
     *
     * @param simpleClassName 完整或简单类名
     * @param methodName      方法名
     * @param methodHash      方法HASH+长度
     * @return
     */
    public static String genCallGraphMethodFileName(String simpleClassName, String methodName, String methodHash) {
        return simpleClassName + JACGConstants.FLAG_AT +
                JACGClassMethodUtil.genSafeMethodName(methodName) + JACGConstants.FLAG_AT +
                methodHash;
    }

    /**
     * 生成内容为空的调用链文件名
     * 格式： {完整或简单类名}@{方法名}
     *
     * @param simpleClassName 完整或简单类名
     * @param methodName      方法名
     * @return
     */
    public static String genEmptyCallGraphFileName(String simpleClassName, String methodName) {
        return simpleClassName + JACGConstants.FLAG_AT + JACGClassMethodUtil.genSafeMethodName(methodName) + JACGConstants.EMPTY_TXT;
    }

    /**
     * 生成代表任务中指定的类或方法不存在的文件
     * 格式： {完整或简单类名}@{任务中指定的方法名}
     *
     * @param simpleClassName 完整或简单类名
     * @param methodName      方法名
     * @return
     */
    public static String genNotFoundGraphFileName(String simpleClassName, String methodName) {
        return simpleClassName + JACGConstants.FLAG_AT + JACGClassMethodUtil.genSafeMethodName(methodName) + JACGConstants.NOT_FOUND_TXT;
    }

    /**
     * 生成代表任务中指定的类或方法不存在的文件
     * 格式： {完整或简单类名}@{任务中指定的方法名}
     *
     * @param taskInfo 任务中指定的类或方法信息
     * @return
     */
    public static String genNotFoundGraphFileName(String taskInfo) {
        return JACGClassMethodUtil.genSafeMethodName(taskInfo) + JACGConstants.NOT_FOUND_TXT;
    }

    /**
     * 判断是否代表空的调用链文件
     *
     * @param fileName 文件名
     * @return
     */
    public static boolean isEmptyCallGraphFileName(String fileName) {
        return StringUtils.endsWith(fileName, JACGConstants.EMPTY_MD);
    }

    /**
     * 判断是否代表方法不存在的调用链文件
     *
     * @param fileName 文件名
     * @return
     */
    public static boolean isNotFoundCallGraphFileName(String fileName) {
        return StringUtils.endsWith(fileName, JACGConstants.NOT_FOUND_MD);
    }

    /**
     * 从方法对应的调用链文件路径，获取方法HASH+长度
     *
     * @param fileName
     * @return
     */
    public static String getMethodHashFromFileName(String fileName) {
        if (fileName == null) {
            return null;
        }

        String fileNameWithOutExt = JACGFileUtil.getFileNameWithOutExt(fileName);
        // 以下文件名的格式，见RunnerGenAllGraph4Caller.handleOneTask()方法
        String[] array = StringUtils.splitPreserveAllTokens(fileNameWithOutExt, JACGConstants.FLAG_AT);
        if (array.length == JACGConstants.CALL_GRAPH_FILE_NAME_COLUMNS_1) {
            return fileNameWithOutExt;
        }
        if (array.length == JACGConstants.CALL_GRAPH_FILE_NAME_COLUMNS_3) {
            return array[2];
        }
        logger.error("调用堆栈文件名使用{}分隔后，列数不符合预期 {} {}", JACGConstants.FLAG_AT, array.length, fileName);
        return null;
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
            throw new JavaCG2RuntimeException("指定的方法级别非法 " + level);
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
        return genOutputLevelFlag(level) + JavaCG2Constants.FLAG_HASHTAG + genOutputLevelSpaceFlag(level);
    }

    /**
     * 生成循环调用标志
     *
     * @param cycleCallLevel
     * @return
     */
    public static String genCycleCallFlag(int cycleCallLevel) {
        return String.format(JACGConstants.CALL_FLAG_CYCLE, cycleCallLevel);
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
            throw new JavaCG2RuntimeException("传入的行内容为空");
        }

        String[] array = StringUtils.splitPreserveAllTokens(line, JavaCG2Constants.FLAG_TAB);
        if (calleeGraph) {
            if (array.length < JACGConstants.CALL_GRAPH_EE_LINE_MIN_COLUMN_NUM) {
                throw new JavaCG2RuntimeException("向上的方法完整调用链文件分隔后列数太小 " + array.length);
            }
        } else if (array.length < JACGConstants.CALL_GRAPH_ER_LINE_MIN_COLUMN_NUM) {
            throw new JavaCG2RuntimeException("向下的方法完整调用链文件分隔后列数太小 " + array.length);
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
        return StringUtils.substringAfter(column1, JavaCG2Constants.FLAG_HASHTAG);
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
        String[] lineColumns = splitCallGraphLine(line, true);
        String column1 = lineColumns[0];
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
            int indexHashTag = column1.indexOf(JavaCG2Constants.FLAG_HASHTAG);
            if (indexHashTag == -1) {
                throw new JavaCG2RuntimeException(line + " 未找到字符 " + JavaCG2Constants.FLAG_HASHTAG);
            }
            String column1SubString = column1.substring(indexHashTag + JavaCG2Constants.FLAG_HASHTAG.length());
            // 获取第1个非空格开始的子字符串
            fullMethodWithAnnotations = JACGUtil.getFirstExcludeSubString(column1SubString, JACGConstants.FLAG_CHAR_SPACE);
            nextStartIndex = 2;
        }
        // 为方法完整调用链解析每行包含的内容
        CallGraphLineParsed callGraphLineParsed = parseCallGraphLine(line, methodLevel, fullMethodWithAnnotations, lineColumns, nextStartIndex);
        // 获取调用方法代码行号
        for (String column : lineColumns) {
            if (column.startsWith(JavaCG2Constants.FLAG_LEFT_BRACKET) && column.endsWith(JavaCG2Constants.FLAG_RIGHT_BRACKET)) {
                int indexStart = column.indexOf(JavaCG2Constants.FLAG_COLON);
                String lineNumber = column.substring(indexStart + JavaCG2Constants.FLAG_COLON.length(), column.length() - JavaCG2Constants.FLAG_RIGHT_BRACKET.length());
                callGraphLineParsed.setCallerLineNumber(Integer.valueOf(lineNumber));
                break;
            }
        }
        return callGraphLineParsed;
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
        String[] lineColumns = splitCallGraphLine(line, false);
        String fullMethodWithAnnotations;
        int nextStartIndex;

        // 获取方法级别
        int methodLevel = getMethodLevel(line);
        // 包含调用方法代码行号的内容
        String contentContainsLineNumber = null;
        if (methodLevel == JACGConstants.CALL_GRAPH_METHOD_LEVEL_START) {
            // 获取方法级别为0的完整方法及注解
            fullMethodWithAnnotations = getFullMethodWithAnnotations4Level0(lineColumns[0]);
            nextStartIndex = 1;
        } else {
            // 方法级别大于0
            contentContainsLineNumber = lineColumns[0];
            fullMethodWithAnnotations = lineColumns[1];
            nextStartIndex = 2;
        }
        // 为方法完整调用链解析每行包含的内容
        CallGraphLineParsed callGraphLineParsed = parseCallGraphLine(line, methodLevel, fullMethodWithAnnotations, lineColumns, nextStartIndex);
        if (contentContainsLineNumber != null) {
            // 获取调用方法代码行号
            int indexStart = contentContainsLineNumber.indexOf(JavaCG2Constants.FLAG_COLON);
            String lineNumber = contentContainsLineNumber.substring(indexStart + JavaCG2Constants.FLAG_COLON.length(),
                    contentContainsLineNumber.length() - JACGConstants.FLAG_RIGHT_PARENTHESES.length());
            callGraphLineParsed.setCallerLineNumber(Integer.valueOf(lineNumber));
        }
        return callGraphLineParsed;
    }

    /**
     * 获取方法级别
     *
     * @param line
     * @return
     */
    public static int getMethodLevel(String line) {
        String level = StringUtils.substringBetween(line, JACGConstants.FLAG_LEFT_PARENTHESES, JACGConstants.FLAG_RIGHT_PARENTHESES);
        if (!JavaCG2Util.isNumStr(level)) {
            throw new JavaCG2RuntimeException("方法调用行内容非法 " + line);
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
     * @param nextStartIndex            后续内容起始序号
     * @return
     */
    private static CallGraphLineParsed parseCallGraphLine(String line, int methodLevel, String fullMethodWithAnnotations, String[] lineColumns, int nextStartIndex) {
        if (fullMethodWithAnnotations == null) {
            throw new JavaCG2RuntimeException("获取方法与注解信息失败 " + line);
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
                String level = StringUtils.substringBetween(column, JACGConstants.FLAG_LEFT_PARENTHESES, JACGConstants.FLAG_RIGHT_PARENTHESES);
                if (level == null) {
                    throw new JavaCG2RuntimeException("方法调用行内容非法 " + line);
                }
                callGraphLineParsed.setCycleCallLevel(Integer.valueOf(level));
            } else if (JACGConstants.CALLEE_FLAG_ENTRY_NO_TAB.equals(column)) {
                // 入口方法
                callGraphLineParsed.setEntryMethodFlag(true);
            } else if (JACGConstants.CALL_FLAG_RUN_IN_OTHER_THREAD_NO_TAB.equals(column)) {
                // 在其他线程中执行
                callGraphLineParsed.setRunInOtherThreadFlag(true);
            } else if (JACGConstants.CALL_FLAG_RUN_IN_SPRING_TX_NO_TAB.equals(column)) {
                // 在事务中执行
                callGraphLineParsed.setRunInTransactionFlag(true);
            }
        }
        callGraphLineParsed.setBusinessDataList(businessDataList);
        return callGraphLineParsed;
    }

    /**
     * 判断是否包含在其他线程中执行的标志（不对行数据进行完整解析）
     *
     * @param line
     * @return
     */
    public static boolean checkRunInOtherThread(String line) {
        return StringUtils.contains(line, JACGConstants.CALL_FLAG_RUN_IN_OTHER_THREAD);
    }

    /**
     * 判断是否包含在事务中执行的标志（不对行数据进行完整解析）
     *
     * @param line
     * @return
     */
    public static boolean checkRunInTransaction(String line) {
        return StringUtils.contains(line, JACGConstants.CALL_FLAG_RUN_IN_SPRING_TX);
    }

    // 处理完整方法及注解
    private static void handleFullMethodWithAnnotations(CallGraphLineParsed callGraphLineParsed, String fullMethodWithAnnotations) {
        int index = fullMethodWithAnnotations.indexOf(JACGConstants.FLAG_AT);
        String fullMethodWithReturnType;
        if (index == -1) {
            // 方法上不存在注解
            fullMethodWithReturnType = fullMethodWithAnnotations;
        } else {
            // 方法上存在注解
            fullMethodWithReturnType = fullMethodWithAnnotations.substring(0, index);
            String annotations = fullMethodWithAnnotations.substring(index + JACGConstants.FLAG_AT.length());
            String[] annotationArray = StringUtils.splitPreserveAllTokens(annotations, JACGConstants.FLAG_AT);
            callGraphLineParsed.setAnnotations(annotationArray);
        }
        MethodDetail methodDetail = JACGClassMethodUtil.genMethodDetail(fullMethodWithReturnType);
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
