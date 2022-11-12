package com.adrninistrator.jacg.util;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.dto.method.MethodInfoInFileName;
import com.adrninistrator.jacg.extensions.dto.extened_data.BaseExtendedData;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author adrninistrator
 * @date 2022/8/24
 * @description: 用于处理生成的调用链文件的工具类
 */
public class JACGCallGraphFileUtil {
    private static final Logger logger = LoggerFactory.getLogger(JACGCallGraphFileUtil.class);

    /**
     * 判断调用链搜索结果文件中指定行是否为序号对应的行，以#开头
     *
     * @param line 文件行内容
     * @return
     */
    public static boolean isDataSeqLine(String line) {
        return StringUtils.startsWith(line, JACGConstants.FLAG_HASHTAG) && StringUtils.contains(line, JACGConstants.FLAG_MD_LINE_NUMBER);
    }

    /**
     * 调用链搜索结果文件中序号对应行的序号值
     *
     * @param line       文件行内容
     * @param lineNumber 当前行号
     * @return
     */
    public static int getDataSeqFromLine(String line, int lineNumber) {
        int spaceIndex = line.indexOf(JACGConstants.FLAG_SPACE);
        int dotIndex = line.indexOf(JACGConstants.FLAG_DOT);
        if (spaceIndex == -1 || dotIndex == -1 || spaceIndex > dotIndex) {
            logger.error("第 {} 行内容非法 {}", lineNumber, line);
            return JACGConstants.DATA_SEQ_NONE;
        }

        String strDataSeq = line.substring(spaceIndex + JACGConstants.FLAG_SPACE.length(), dotIndex);
        return Integer.parseInt(strDataSeq);
    }

    /**
     * 判断是否为方法完整调用链的行
     *
     * @param line
     * @return
     */
    public static boolean isCallGraphLine(String line) {
        return StringUtils.startsWith(line, JACGConstants.FLAG_LEFT_PARENTHESES);
    }

    /**
     * 判断调用链搜索结果文件行内容是否包含自定义数据
     *
     * @param line 文件行内容
     * @return
     */
    public static boolean checkLineContainsExtendedData(String line) {
        return isCallGraphLine(line) &&
                (StringUtils.contains(line, JACGConstants.CALL_FLAG_EXTENDED_DATA) || StringUtils.contains(line, JACGConstants.CALL_FLAG_EXTENDED_DATA_MANUAL_ADD));
    }

    /**
     * 从调用链搜索结果文件行内容获取自定义数据字符串
     *
     * @param line 文件行内容
     * @return
     */
    public static String getExtendedDataStringFromLine(String line) {
        String[] array = line.split(JACGConstants.FLAG_TAB);
        for (String str : array) {
            String extendedData = null;

            if (str.startsWith(JACGConstants.CALL_FLAG_EXTENDED_DATA_NO_TAB)) {
                extendedData = str.substring(JACGConstants.CALL_FLAG_EXTENDED_DATA_NO_TAB.length());
            } else if (str.startsWith(JACGConstants.CALL_FLAG_EXTENDED_DATA_MANUAL_ADD_NO_TAB)) {
                extendedData = str.substring(JACGConstants.CALL_FLAG_EXTENDED_DATA_MANUAL_ADD_NO_TAB.length());
            }

            if (extendedData != null) {
                return extendedData;
            }
        }

        logger.error("未找到自定义数据 {}", line);
        return null;
    }

    /**
     * 根据自定义数据字符串获取自定义数据
     *
     * @param extendedDataString 自定义数据字符串
     * @return
     */
    public static BaseExtendedData getExtendedDataFromString(String extendedDataString) {
        int atIndex = extendedDataString.indexOf(JACGConstants.FLAG_AT);
        if (atIndex == -1) {
            logger.error("自定义数据中未找到{}字符 {}", JACGConstants.FLAG_AT, extendedDataString);
            return null;
        }

        String dataType = extendedDataString.substring(0, atIndex);
        String dataValue = extendedDataString.substring(atIndex + JACGConstants.FLAG_AT.length());

        BaseExtendedData extendedData = new BaseExtendedData();
        extendedData.setDataType(dataType);
        extendedData.setDataValue(dataValue);
        return extendedData;
    }

    /**
     * 根据调用链搜索结果文件行内容获取自定义数据
     *
     * @param line 文件行内容
     * @return
     */
    public static BaseExtendedData getExtendedDataFromLine(String line) {
        String extendedDataString = getExtendedDataStringFromLine(line);
        if (extendedDataString == null) {
            return null;
        }

        return getExtendedDataFromString(extendedDataString);
    }

    /**
     * 根据向下的调用链文件行内容获取被调用方法
     *
     * @param line 文件行内容
     * @return
     */
    public static String getCalleeMethodFromCallerGraph(String line) {
        /*
            文件行内容示例：
            [0]#test.example.service.impl.Service1Impl:test1()@test.example.annotation.TestAnnotation1
            [1]#  [Service1Impl:56]	test.example.rpc.wrapper.RpcSenderWrapperManualAdd:sendWrapperByArguments1(java.lang.String,java.lang.String,java.lang.String,long)
            [2]#    [RpcSenderWrapperManualAdd:14]	test.example.rpc.sender.RpcSenderByArguments:send1(java.lang.String,java.lang.String,java.lang.String,long)
            !ext_data!RPC_ARG@{"serviceNameList":[],"versionList":[],"timeout":"#未获取到#"}
         */
        if (line == null) {
            return null;
        }

        // 获取第1个#之后的内容
        int indexHashTag = line.indexOf(JACGConstants.FLAG_HASHTAG);
        if (indexHashTag == -1) {
            logger.error("文件行内容使用未找到{}字符 {}", JACGConstants.FLAG_HASHTAG, line);
            return null;
        }
        String line2 = line.substring(indexHashTag + JACGConstants.FLAG_HASHTAG.length());

        // 有的方法上有注解信息，获取剩余内容第1个@之前的内容
        String line3 = JACGUtil.getFirstSubString(line2, JACGConstants.FLAG_AT);

        // 对剩余内容使用\t进行分隔
        String[] array3 = line3.split(JACGConstants.FLAG_TAB);
        if (array3.length == 1) {
            // 剩余内容使用\t分隔后只有一列，说明为第1行，直接使用
            return line3;
        }

        // 剩余内容使用\t分隔后多于一列，使用第2列
        return array3[1];
    }

    /**
     * 根据向上的调用链文件行内容获取调用方法及注解信息
     *
     * @param line 文件行内容
     * @return
     */
    public static String getCallerMethodWithAnnotationsFromCalleeGraph(String line) {
         /*
            文件行内容示例：
            [2]#    test.call_graph.future.TestFuture:test1()	(TestFuture:16)	!entry!
            [1]#  test.call_graph.future.FutureImpl:get()	(FutureImpl:32)
            [0]#java.lang.System:currentTimeMillis()

            [7]#              a.b.CController:method(Request,Response)@org.springframework.web.bind.annotation.RequestMapping(/path1/path2.do)	(CController:55)	!entry!
            [6]#            a.b.CController$1:<init>(a.b.CController)	(CController$1:0)
            [5]#      a.b.CService:query(a.b.CDTO)	(CService:0)
         */
        if (line == null) {
            return null;
        }

        // 获取第1个#之后的内容
        int indexHashTag = line.indexOf(JACGConstants.FLAG_HASHTAG);
        if (indexHashTag == -1) {
            logger.error("文件行内容使用未找到{}字符 {}", JACGConstants.FLAG_HASHTAG, line);
            return null;
        }
        String line2 = line.substring(indexHashTag + JACGConstants.FLAG_HASHTAG.length());

        // 找到第1个非空格开始的子字符串
        String line3 = JACGUtil.getFirstExcludeSubString(line2, JACGConstants.FLAG_CHAR_SPACE);
        if (line3 == null) {
            logger.error("文件行内容未找到非{}的字符 {}", JACGConstants.FLAG_CHAR_SPACE, line);
            return null;
        }

        // 获取第一个\t之前的数据（后续可能存在注解信息，在将注解信息写向调用链文件时，会将TAB替换为空格，半角@替换为全角＠，可以避免产生影响）
        int indexTab = line3.indexOf(JACGConstants.FLAG_TAB);
        if (indexTab == -1) {
            // 对于级别为0的行，后面没有TAB，直接返回
            return line3;
        }

        return line3.substring(0, indexTab);
    }

    /**
     * 根据向上的调用链文件行内容获取调用方法
     *
     * @param line 文件行内容
     * @return
     */
    public static String getCallerMethodFromCalleeGraph(String line) {
        String line1 = getCallerMethodWithAnnotationsFromCalleeGraph(line);
        if (line1 == null) {
            return null;
        }

        // 使用@进行分隔，取第1列，去掉注解
        return JACGUtil.getFirstSubString(line1, JACGConstants.FLAG_AT);
    }

    /**
     * 根据向上的调用链文件行内容获取注解信息
     * 假如一个方法上有多个注解，则返回信息里也包含多个注解信息，如@xxx@yyy
     *
     * @param line 文件行内容
     * @return
     */
    public static String getCallerAnnotationsFromCalleeGraph(String line) {
        String line1 = getCallerMethodWithAnnotationsFromCalleeGraph(line);
        if (line1 == null) {
            return null;
        }

        int indexAt = line1.indexOf(JACGConstants.FLAG_AT);
        if (indexAt == -1) {
            // 调用者方法行中不包含@，即没有注解信息
            return null;
        }
        // 获取第1个@及之后的内容
        return line1.substring(indexAt);
    }

    /**
     * 从包含一个或多个注解的信息中，获取指定的注解信息
     * 传入的注解信息示例如下： @xxx @xxx@yyy
     *
     * @param annotationsInfo 一个或多个注解信息
     * @param annotationName  需要获取的类名
     * @return
     */
    public static String getAnnotationFromAnnotations(String annotationsInfo, String annotationName) {
        if (annotationsInfo == null || annotationName == null) {
            return null;
        }

        String[] array = annotationsInfo.split(JACGConstants.FLAG_AT);
        for (String annotationInfo : array) {
            if (annotationInfo.startsWith(annotationName)) {
                return JACGConstants.FLAG_AT + annotationInfo;
            }
        }

        return null;
    }

    /**
     * 从方法完整调用链文件行中，获取当前方法的级别
     * 文件行示例 [0]#java.lang.System:setProperty
     *
     * @param line
     * @return
     */
    public static int getCallGraphMethodLevel(String line) {
        if (line == null) {
            return JACGConstants.CALL_GRAPH_METHOD_LEVEL_ILLEGAL;
        }

        int indexLeft = line.indexOf(JACGConstants.FLAG_LEFT_PARENTHESES);
        int indexRight = line.indexOf(JACGConstants.FLAG_RIGHT_PARENTHESES);
        if (indexLeft == -1 || indexRight == -1 || indexLeft >= indexRight) {
            logger.error("文件行内容非法 {}", line);
            return JACGConstants.CALL_GRAPH_METHOD_LEVEL_ILLEGAL;
        }

        String methodLevelStr = line.substring(indexLeft + JACGConstants.FLAG_LEFT_PARENTHESES_LENGTH, indexRight);
        if (!JACGUtil.isValidNum(methodLevelStr)) {
            logger.error("文件行内容方法级别非法 {} {}", methodLevelStr, line);
            return JACGConstants.CALL_GRAPH_METHOD_LEVEL_ILLEGAL;
        }

        return Integer.parseInt(methodLevelStr);
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
                JACGUtil.getSafeMethodName(methodName) + JACGConstants.FLAG_AT +
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
        return simpleClassName + JACGConstants.FLAG_AT + JACGUtil.getSafeMethodName(methodName) + JACGConstants.EXT_EMPTY_TXT;
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

        String fileNameWithOutExt = JACGUtil.getFileNameWithOutExt(fileName);
        // 以下文件名的格式，见RunnerGenAllGraph4Caller.handleOneTask()方法
        String[] array = fileNameWithOutExt.split(JACGConstants.FLAG_AT);
        if (array.length < JACGConstants.CALLER_FILE_NAME_SPLIT_BY_AT_MIN_COLUMNS) {
            // 因为文件名中可能包括代码行号，会多一个使用@分隔的数据，因此分隔后的列长度可能会更多，但不能更少
            logger.error("文件名使用{}分隔后，列数小于 {} {}", JACGConstants.FLAG_AT, JACGConstants.CALLER_FILE_NAME_SPLIT_BY_AT_MIN_COLUMNS, fileName);
            return null;
        }

        return new MethodInfoInFileName(array[0], JACGUtil.recoveryMethodName(array[1]), array[2]);
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
        return JACGUtil.getFirstSubString(fileName, JACGConstants.FLAG_AT);
    }

    private JACGCallGraphFileUtil() {
        throw new IllegalStateException("illegal");
    }
}
