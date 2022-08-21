package com.adrninistrator.jacg.extensions.extractor;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.common.enums.OutputDetailEnum;
import com.adrninistrator.jacg.conf.ConfInfo;
import com.adrninistrator.jacg.conf.ConfManager;
import com.adrninistrator.jacg.extensions.dto.ExtendedDataFile;
import com.adrninistrator.jacg.extensions.dto.ExtendedDataInfo;
import com.adrninistrator.jacg.extensions.dto.MultiImplMethodData;
import com.adrninistrator.jacg.extensions.find_filter.BaseFindKeywordFilter;
import com.adrninistrator.jacg.extensions.util.JsonUtil;
import com.adrninistrator.jacg.other.FindKeywordCallGraph;
import com.adrninistrator.jacg.other.GenSingleCallGraph;
import com.adrninistrator.jacg.util.JACGUtil;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2021/10/16
 * @description:
 */
public class ExtendedDataExtractor {

    private static final Logger logger = LoggerFactory.getLogger(ExtendedDataExtractor.class);

    // 关键字搜索自定义过滤处理类
    private BaseFindKeywordFilter baseFindKeywordFilter;

    // 保存当前查找关键字对应的结果目录
    private String currentFindResultDirPath;

    /**
     * 生成向下的完整调用链，根据关键字进行查找，获取自定义数据并返回
     *
     * @return
     */
    public List<ExtendedDataFile> extract() {
        // 设置生成的调用链顺序为向下
        GenSingleCallGraph.setOrder4er();
        // 判断生成调用链时的详细程度是否为最详细
        ConfInfo confInfo = ConfManager.getConfInfo();
        if (confInfo == null) {
            logger.error("配置信息为空");
            return null;
        }

        if (!OutputDetailEnum.ODE_1.getDetail().equals(confInfo.getCallGraphOutputDetail())) {
            logger.error("生成调用链时的详细程度需要设置为最详细 {} {}", ConfigKeyEnum.CKE_CALL_GRAPH_OUTPUT_DETAIL.getKey(), OutputDetailEnum.ODE_1.getDetail());
            return null;
        }

        FindKeywordCallGraph findKeywordCallGraph = new FindKeywordCallGraph();
        // 设置处理目录时，需要返回生成文件路径列表
        findKeywordCallGraph.setReturnResultFileList();

        if (!confInfo.isMultiImplGenInCurrentFile()) {
            // 生成向下的调用链时，若接口或父类存在多个实现类或子类，接口或父类方法调用多个实现类或子类方法的调用关系需要在单独的目录中生成
            // 添加额外关键字
            findKeywordCallGraph.addExtraKeyword(JACGConstants.CALL_FLAG_EXTENDED_DATA + JACGConstants.DATA_TYPE_JUMP_MULTI_IMPL + JACGConstants.FLAG_AT);
        }
        // 添加关键字过滤处理类
        findKeywordCallGraph.setBaseFindKeywordFilter(baseFindKeywordFilter);
        // 设置生成目录名使用固定值，不指定时间
        findKeywordCallGraph.setOutputDirUseFixedValue(true);

        List<String> resultFileList = findKeywordCallGraph.find(new String[]{});
        if (resultFileList == null) {
            logger.error("生成向下的方法调用链及查找关键字失败");
            return null;
        }

        // 获取当前查找关键字对应的结果目录
        currentFindResultDirPath = findKeywordCallGraph.getCurrentDirPath();

        List<ExtendedDataFile> extendedDataFileList = new ArrayList<>(resultFileList.size());

        for (String resultFile : resultFileList) {
            // 处理文件中的自定义数据
            ExtendedDataFile extendedDataFile = handleExtendedDataInFile(resultFile, currentFindResultDirPath);
            if (extendedDataFile == null) {
                return null;
            }

            extendedDataFileList.add(extendedDataFile);
        }

        return extendedDataFileList;
    }

    // 返回当前查找关键字对应的结果目录
    public String getCurrentFindResultDirPath() {
        return currentFindResultDirPath;
    }

    // 处理文件中的自定义数据
    public ExtendedDataFile handleExtendedDataInFile(String filePath, String currentFindResultDirPath) {
        if (StringUtils.isBlank(filePath)) {
            logger.error("未指定文件路径");
            return null;
        }

        logger.info("处理文件中的自定义数据 {}", filePath);

        int lineNumber = 0;

        // 当前处理的数据序号
        int dataSeq = -1;

        List<ExtendedDataInfo> extendedDataInfoList = new ArrayList<>();

        try (BufferedReader br = new BufferedReader(new InputStreamReader(new FileInputStream(filePath), StandardCharsets.UTF_8))) {
            String line;
            while ((line = br.readLine()) != null) {
                lineNumber++;

                if (line.startsWith(JACGConstants.FLAG_HASHTAG)) {
                    // 读取到#时，说明开始处理一段数据
                    int spaceIndex = line.indexOf(JACGConstants.FLAG_SPACE);
                    int dotIndex = line.indexOf(JACGConstants.FLAG_DOT);
                    if (spaceIndex == -1 || dotIndex == -1 || spaceIndex > dotIndex) {
                        logger.error("第 {} 行内容非法 {}", lineNumber, line);
                        return null;
                    }

                    String strDataSeq = line.substring(spaceIndex + JACGConstants.FLAG_SPACE.length(), dotIndex);
                    dataSeq = Integer.parseInt(strDataSeq);
                    continue;
                }

                if (dataSeq == -1) {
                    // 上一段数据处理完毕，还未开始处理下一段数据时，跳过
                    continue;
                }

                if (line.startsWith(JACGConstants.FLAG_LEFT_PARENTHESES) && (
                        line.contains(JACGConstants.CALL_FLAG_EXTENDED_DATA) || line.contains(JACGConstants.CALL_FLAG_EXTENDED_DATA_MANUAL_ADD))) {
                    // 当前行包含自定义数据，进行处理
                    if (!handleOneExtendedData(line, dataSeq, lineNumber, extendedDataInfoList, currentFindResultDirPath)) {
                        logger.error("行号 {} 自定义数据格式非法 {}", lineNumber, line);
                        return null;
                    }

                    // 当前数据处理完毕，等待后续数据进行处理
                    dataSeq = -1;
                }
            }

            ExtendedDataFile extendedDataFile = new ExtendedDataFile();
            extendedDataFile.setFilePath(filePath);
            int lastSeparatorIndex = filePath.lastIndexOf(File.separator);
            extendedDataFile.setFileName(filePath.substring(lastSeparatorIndex + File.separator.length()));
            extendedDataFile.setExtendedDataInfoList(extendedDataInfoList);

            return extendedDataFile;
        } catch (Exception e) {
            logger.error("error ", e);
            return null;
        }
    }

    // 处理一条自定义数据
    private boolean handleOneExtendedData(String line, int dataSeq, int lineNumber, List<ExtendedDataInfo> extendedDataInfoList, String currentFindResultDirPath) {

        String[] array = line.split(JACGConstants.FLAG_TAB);
        for (String str : array) {
            String extendedData = null;

            if (str.startsWith(JACGConstants.CALL_FLAG_EXTENDED_DATA_NO_TAB)) {
                extendedData = str.substring(JACGConstants.CALL_FLAG_EXTENDED_DATA_NO_TAB.length());
            } else if (str.startsWith(JACGConstants.CALL_FLAG_EXTENDED_DATA_MANUAL_ADD_NO_TAB)) {
                extendedData = str.substring(JACGConstants.CALL_FLAG_EXTENDED_DATA_MANUAL_ADD_NO_TAB.length());
            }

            if (extendedData != null) {
                // 找到自定义数据
                int atIndex = extendedData.indexOf(JACGConstants.FLAG_AT);
                if (atIndex == -1) {
                    logger.error("行号 {} {} 自定义数据中未找到标志 {}", lineNumber, line, JACGConstants.FLAG_AT);
                    return false;
                }

                String dataType = extendedData.substring(0, atIndex);
                String dataValue = extendedData.substring(atIndex + JACGConstants.FLAG_AT.length());

                if (JACGConstants.DATA_TYPE_JUMP_MULTI_IMPL.equals(dataType)) {
                    // 处理存在多个实现类的接口或父类方法
                    handleMultiImplMethod(dataType, dataValue, dataSeq, lineNumber, extendedDataInfoList, currentFindResultDirPath);
                } else {
                    ExtendedDataInfo extendedDataInfo = new ExtendedDataInfo();
                    extendedDataInfo.setDataType(dataType);
                    extendedDataInfo.setDataValue(dataValue);
                    extendedDataInfo.setDataSeq(dataSeq);
                    extendedDataInfo.setLineNumber(lineNumber);

                    extendedDataInfoList.add(extendedDataInfo);
                }

                break;
            }
        }
        return true;
    }

    // 处理存在多个实现类的接口或父类方法
    private void handleMultiImplMethod(String dataType, String interfaceOrSuperMethodName, int dataSeq, int lineNumber, List<ExtendedDataInfo> extendedDataInfoList,
                                       String currentFindResultDirPath) {
        String multiImplMethodDirPath = currentFindResultDirPath + File.separator + interfaceOrSuperMethodName;
        logger.info("处理存在多个实现类的接口或父类方法 {}", multiImplMethodDirPath);
        File dir = new File(multiImplMethodDirPath);
        File[] files = dir.listFiles();
        if (files == null) {
            return;
        }

        int implSeq = 0;
        for (File file : files) {
            if (file.isFile() && file.length() > 0) {
                String fileName = file.getName();
                if (fileName.endsWith(JACGConstants.EXT_MD)) {
                    ExtendedDataInfo extendedDataInfo = new ExtendedDataInfo();
                    extendedDataInfo.setDataType(dataType);

                    String implMethodName = fileName.substring(0, fileName.length() - JACGConstants.EXT_MD.length());

                    MultiImplMethodData multiImplMethodData = new MultiImplMethodData();
                    // 转换方法名格式
                    multiImplMethodData.setInterfaceOrSuperMethodName(JACGUtil.getMethodNameFromFileName(interfaceOrSuperMethodName));
                    multiImplMethodData.setImplSeq(++implSeq);
                    // 转换方法名格式
                    multiImplMethodData.setImplMethodName(JACGUtil.getMethodNameFromFileName(implMethodName));
                    multiImplMethodData.setImplMethodMdFilePath(file.getAbsolutePath());

                    extendedDataInfo.setDataValue(JsonUtil.getJsonStr(multiImplMethodData));
                    extendedDataInfo.setDataSeq(dataSeq);
                    extendedDataInfo.setLineNumber(lineNumber);

                    extendedDataInfoList.add(extendedDataInfo);
                }
            }
        }
    }

    // 设置关键字搜索自定义过滤处理类
    public void setBaseFindKeywordFilter(BaseFindKeywordFilter baseFindKeywordFilter) {
        this.baseFindKeywordFilter = baseFindKeywordFilter;
    }
}