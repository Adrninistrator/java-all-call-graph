package com.adrninistrator.jacg.extractor.entry;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.extensions.dto.extened_data.BaseExtendedData;
import com.adrninistrator.jacg.extensions.dto.multi_impl.MultiImplMethodData;
import com.adrninistrator.jacg.extensions.util.JsonUtil;
import com.adrninistrator.jacg.extractor.dto.result.CallerExtendedDataFile;
import com.adrninistrator.jacg.extractor.dto.result.CallerExtendedDataInfo;
import com.adrninistrator.jacg.util.JACGCallGraphFileUtil;
import com.adrninistrator.jacg.util.JACGFileUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedReader;
import java.io.File;
import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2021/10/16
 * @description: 对调用链结果文件进行数据提取，向下的方法调用链，获取自定义数据
 */
public class CallerGraphExtendedDataExtractor extends BaseExtractor {
    private static final Logger logger = LoggerFactory.getLogger(CallerGraphExtendedDataExtractor.class);

    /**
     * 生成向下的完整调用链，根据关键字进行查找，获取自定义数据并返回
     *
     * @return
     */
    public List<CallerExtendedDataFile> extract() {
        return extract(new ConfigureWrapper());
    }

    /**
     * 生成向下的完整调用链，根据关键字进行查找，获取自定义数据并返回
     *
     * @param configureWrapper
     * @return
     */
    public List<CallerExtendedDataFile> extract(ConfigureWrapper configureWrapper) {
        if (!init(configureWrapper)) {
            return null;
        }

        if (!confInfo.isMultiImplGenInCurrentFile()) {
            // 生成向下的调用链时，若接口或父类存在多个实现类或子类，接口或父类方法调用多个实现类或子类方法的调用关系需要在单独的目录中生成
            // 添加额外关键字
            findKeywordCallGraph.addExtraKeyword(JACGConstants.CALL_FLAG_EXTENDED_DATA + JACGConstants.DATA_TYPE_JUMP_MULTI_IMPL + JACGConstants.FLAG_AT);
        }

        // 处理向下的方法调用链
        List<String> resultFilePathList = findKeywordCallGraph.find(chooseFindKeywordCallGraph(), configureWrapper);
        if (resultFilePathList == null) {
            logger.error("生成向下的方法调用链及查找关键字失败");
            return null;
        }

        // 创建数据库相关对象
        if (!genDbObject()) {
            return null;
        }

        // 获取当前查找关键字对应的结果目录
        currentFindResultDirPath = findKeywordCallGraph.getCurrentDirPath();

        List<CallerExtendedDataFile> callerExtendedDataFileList = new ArrayList<>(resultFilePathList.size());
        for (String resultFilePath : resultFilePathList) {
            // 处理文件中的自定义数据
            CallerExtendedDataFile callerExtendedDataFile = handleExtendedDataInFile(resultFilePath);
            if (callerExtendedDataFile == null) {
                return null;
            }
            callerExtendedDataFileList.add(callerExtendedDataFile);
        }

        // 关闭数据源
        closeDs();
        return callerExtendedDataFileList;
    }

    // 处理文件中的自定义数据
    public CallerExtendedDataFile handleExtendedDataInFile(String filePath) {
        if (StringUtils.isBlank(filePath)) {
            logger.error("未指定文件路径");
            return null;
        }

        logger.info("处理文件中的自定义数据 {}", filePath);

        int lineNumber = 0;

        // 当前处理的数据序号
        int dataSeq = JACGConstants.DATA_SEQ_NONE;

        // 当前是否处理到一段数据
        boolean handleData = false;

        List<CallerExtendedDataInfo> callerExtendedDataInfoList = new ArrayList<>();
        try (BufferedReader br = JACGFileUtil.genBufferedReader(filePath)) {
            String line;
            String lastLine = null;
            while ((line = br.readLine()) != null) {
                lineNumber++;

                if (JACGCallGraphFileUtil.isDataSeqLine(line)) {
                    // 读取到#时，说明开始处理一段数据
                    dataSeq = JACGCallGraphFileUtil.getDataSeqFromLine(line, lineNumber);
                    if (dataSeq == JACGConstants.DATA_SEQ_NONE) {
                        return null;
                    }

                    // 标记开始处理数据
                    handleData = true;

                    // 清空上一行内容
                    lastLine = null;
                    continue;
                }

                if (!handleData) {
                    // 上一段数据处理完毕，还未开始处理下一段数据时，跳过
                    continue;
                }

                if (JACGCallGraphFileUtil.checkLineContainsExtendedData(line)) {
                    // 当前行包含自定义数据，进行处理
                    if (!handleOneExtendedData(line, lastLine, dataSeq, lineNumber, callerExtendedDataInfoList)) {
                        logger.error("行号 {} 自定义数据格式非法 {}", lineNumber, line);
                        return null;
                    }

                    // 当前数据处理完毕，等待后续数据进行处理
                    handleData = false;
                }

                // 记录上一行内容
                lastLine = line;
            }

            return genExtendedDataFile(filePath, callerExtendedDataInfoList);
        } catch (Exception e) {
            logger.error("error ", e);
            return null;
        }
    }

    private CallerExtendedDataFile genExtendedDataFile(String filePath, List<CallerExtendedDataInfo> callerExtendedDataInfoList) {
        CallerExtendedDataFile callerExtendedDataFile = new CallerExtendedDataFile();
        callerExtendedDataFile.setCallerExtendedDataInfoList(callerExtendedDataInfoList);

        // 处理关键字搜索结果文件
        handleResultFile(filePath, callerExtendedDataFile);
        if (callerExtendedDataFile.isEmptyFile()) {
            return callerExtendedDataFile;
        }

        // 根据调用者完整方法HASH+长度，从方法调用表获取对应的完整方法
        String callerFullMethod = dbOperWrapper.getCallerFullMethodFromHash(callerExtendedDataFile.getMethodHash());
        callerExtendedDataFile.setFullMethod(callerFullMethod);
        if (callerFullMethod != null) {
            callerExtendedDataFile.setFullClassName(JACGUtil.getFullClassNameFromMethod(callerFullMethod));
        }
        return callerExtendedDataFile;
    }

    // 处理一条自定义数据
    private boolean handleOneExtendedData(String line,
                                          String lastLine,
                                          int dataSeq,
                                          int lineNumber,
                                          List<CallerExtendedDataInfo> callerExtendedDataInfoList) {
        BaseExtendedData extendedData = JACGCallGraphFileUtil.getExtendedDataFromLine(line);
        if (extendedData == null) {
            logger.error("行号 {} {} 未获取到自定义数据", lineNumber, line);
            return false;
        }

        String dataType = extendedData.getDataType();
        String dataValue = extendedData.getDataValue();

        CallerExtendedDataInfo callerExtendedDataInfo = new CallerExtendedDataInfo();
        callerExtendedDataInfo.setDataType(dataType);
        callerExtendedDataInfo.setDataValue(dataValue);
        callerExtendedDataInfo.setDataSeq(dataSeq);
        callerExtendedDataInfo.setLineNumber(lineNumber);
        callerExtendedDataInfo.setLastLineContent(lastLine);
        callerExtendedDataInfo.setLineContent(line);

        if (JACGConstants.DATA_TYPE_JUMP_MULTI_IMPL.equals(dataType)) {
            // 处理存在多个实现类的接口或父类方法
            handleMultiImplMethod(callerExtendedDataInfo, callerExtendedDataInfoList);
            return true;
        }

        // 不存在多个实现类的接口或父类方法
        // 根据向下的调用链文件行内容获取被调用方法
        String lastLineFullMethod = JACGCallGraphFileUtil.getCalleeMethodFromCallerGraph(lastLine);
        if (lastLineFullMethod == null) {
            return false;
        }
        callerExtendedDataInfo.setLastLineFullMethod(lastLineFullMethod);

        callerExtendedDataInfoList.add(callerExtendedDataInfo);
        return true;
    }

    // 处理存在多个实现类的接口或父类方法
    private void handleMultiImplMethod(CallerExtendedDataInfo currentCallerExtendedDataInfo, List<CallerExtendedDataInfo> callerExtendedDataInfoList) {
        String interfaceOrSuperMethodName = currentCallerExtendedDataInfo.getDataValue();
        String multiImplMethodDirPath = currentFindResultDirPath + File.separator + interfaceOrSuperMethodName;
        logger.info("处理存在多个实现类的接口或父类方法 {}", multiImplMethodDirPath);
        File dir = new File(multiImplMethodDirPath);
        File[] files = dir.listFiles();
        if (files == null) {
            return;
        }

        int implSeq = 0;
        for (File file : files) {
            if (!file.isFile() || file.length() <= 0) {
                continue;
            }

            String fileName = file.getName();
            if (!fileName.endsWith(JACGConstants.EXT_MD)) {
                continue;
            }

            CallerExtendedDataInfo callerExtendedDataInfo = new CallerExtendedDataInfo();
            callerExtendedDataInfo.setDataType(currentCallerExtendedDataInfo.getDataType());

            String implMethodName = fileName.substring(0, fileName.length() - JACGConstants.EXT_MD.length());

            MultiImplMethodData multiImplMethodData = new MultiImplMethodData();
            // 转换方法名格式
            multiImplMethodData.setInterfaceOrSuperMethodName(JACGUtil.getMethodNameFromFileName(interfaceOrSuperMethodName));
            multiImplMethodData.setImplSeq(++implSeq);
            // 转换方法名格式
            multiImplMethodData.setImplMethodName(JACGUtil.getMethodNameFromFileName(implMethodName));
            multiImplMethodData.setImplMethodMdFilePath(file.getAbsolutePath());

            callerExtendedDataInfo.setDataValue(JsonUtil.getJsonStr(multiImplMethodData));
            callerExtendedDataInfo.setDataSeq(currentCallerExtendedDataInfo.getDataSeq());
            callerExtendedDataInfo.setLineNumber(currentCallerExtendedDataInfo.getLineNumber());
            callerExtendedDataInfo.setLastLineContent(currentCallerExtendedDataInfo.getLastLineContent());
            callerExtendedDataInfo.setLineContent(currentCallerExtendedDataInfo.getLineContent());

            callerExtendedDataInfoList.add(callerExtendedDataInfo);
        }
    }

    @Override
    protected boolean chooseFindKeywordCallGraph() {
        // 向下
        return false;
    }
}