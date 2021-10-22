package com.adrninistrator.jacg.extensions.extractor;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.extensions.dto.ExtendedDataFile;
import com.adrninistrator.jacg.extensions.dto.ExtendedDataTypeAndValue;
import com.adrninistrator.jacg.other.FindKeywordCallGraph;
import com.adrninistrator.jacg.other.GenSingleCallGraph;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Caller;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedReader;
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

    /**
     * 生成向下的完整调用链，根据关键字进行查找，获取自定义数据并返回
     *
     * @return
     */
    public List<ExtendedDataFile> extract() {
        // 设置生成的调用链顺序为向下
        GenSingleCallGraph.setOrder4er();
        // 设置处理目录时，需要返回生成文件路径列表
        FindKeywordCallGraph.setReturnResultFileList();
        // 设置生成调用链时的详细程度为最详细
        RunnerGenAllGraph4Caller.setCallGraphOutputDetailMost();

        FindKeywordCallGraph findKeywordCallGraph = new FindKeywordCallGraph();
        List<String> resultFileList = findKeywordCallGraph.find(new String[]{});
        if (resultFileList == null) {
            logger.error("生成向下的方法调用链及查找关键字失败");
            return null;
        }

        List<ExtendedDataFile> extendedDataFileList = new ArrayList<>(resultFileList.size());

        for (String resultFile : resultFileList) {
            // 处理文件中的自定义数据
            List<ExtendedDataTypeAndValue> extendedDataTypeAndValueList = handleExtendedDataInFile(resultFile);
            if (extendedDataTypeAndValueList == null) {
                return null;
            }

            ExtendedDataFile extendedDataFile = new ExtendedDataFile();
            extendedDataFile.setFilePath(resultFile);
            extendedDataFile.setExtendedDataTypeAndValueList(extendedDataTypeAndValueList);

            extendedDataFileList.add(extendedDataFile);
        }

        return extendedDataFileList;
    }

    // 处理文件中的自定义数据
    private List<ExtendedDataTypeAndValue> handleExtendedDataInFile(String file) {
        logger.info("处理文件中的自定义数据 {}", file);

        int lineNumber = 0;

        // 开始处理一段数据标识
        boolean partStart = false;

        List<ExtendedDataTypeAndValue> extendedDataTypeAndValueList = new ArrayList<>();

        try (BufferedReader br = new BufferedReader(new InputStreamReader(new FileInputStream(file), StandardCharsets.UTF_8))) {
            String line;
            while ((line = br.readLine()) != null) {
                lineNumber++;

                if (line.isEmpty()) {
                    // 读取到空行时，说明开始处理一段数据
                    partStart = true;
                    continue;
                }

                if (!partStart) {
                    // 上一段数据处理完毕，还未开始处理下一段数据时，跳过
                    continue;
                }

                if (line.startsWith(JACGConstants.FLAG_LEFT_PARENTHESES) && (
                        line.contains(JACGConstants.CALL_FLAG_EXTENDED_DATA) || line.contains(JACGConstants.CALL_FLAG_EXTENDED_DATA_MANUAL_ADD))) {
                    // 当前行包含自定义数据，进行处理

                    // 记录当前行是否有处理
                    boolean handle = false;

                    String[] array = line.split(JACGConstants.FLAG_TAB);
                    for (String str : array) {
                        String extendedData = null;

                        if (str.startsWith(JACGConstants.CALL_FLAG_EXTENDED_DATA_NO_TAB)) {
                            extendedData = str.substring(JACGConstants.CALL_FLAG_EXTENDED_DATA_NO_TAB.length());
                        } else if (str.startsWith(JACGConstants.CALL_FLAG_EXTENDED_DATA_MANUAL_ADD_NO_TAB)) {
                            extendedData = str.substring(JACGConstants.CALL_FLAG_EXTENDED_DATA_MANUAL_ADD_NO_TAB.length());
                        }

                        if (extendedData == null) {
                            continue;
                        }

                        int atIndex = extendedData.indexOf(JACGConstants.FLAG_AT);
                        if (atIndex == -1) {
                            logger.error("行号 {} {} 自定义数据中未找到标志 {}", lineNumber, line, JACGConstants.FLAG_AT);
                            return null;
                        }

                        String dataType = extendedData.substring(0, atIndex);
                        String dataValue = extendedData.substring(atIndex + JACGConstants.FLAG_AT.length());

                        ExtendedDataTypeAndValue extendedDataTypeAndValue = new ExtendedDataTypeAndValue();
                        extendedDataTypeAndValue.setDataType(dataType);
                        extendedDataTypeAndValue.setDataValue(dataValue);

                        extendedDataTypeAndValueList.add(extendedDataTypeAndValue);

                        handle = true;
                        break;
                    }

                    if (!handle) {
                        logger.error("行号 {} 自定义数据格式非法 {}", lineNumber, line);
                        return null;
                    }

                    // 当前数据处理完毕，等待后续数据进行处理
                    partStart = false;
                }
            }

            return extendedDataTypeAndValueList;
        } catch (Exception e) {
            logger.error("error ", e);
            return null;
        }
    }
}
