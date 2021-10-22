package com.adrninistrator.jacg.other;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Callee;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Caller;
import com.adrninistrator.jacg.runner.base.AbstractRunnerGenCallGraph;
import com.adrninistrator.jacg.util.CommonUtil;
import com.adrninistrator.jacg.util.FileUtil;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.*;

/**
 * @author adrninistrator
 * @date 2021/7/29
 * @description:
 */

public class FindKeywordCallGraph {

    private static final Logger logger = LoggerFactory.getLogger(FindKeywordCallGraph.class);

    public static final String RETURN_RESULT_FILE_LIST_KEY = "return_result_file_list";

    /**
     * 在生成的方法调用链文件中搜索指定关键字
     *
     * @param args 为空时先生成方法调用链文件
     * @return 生成的搜索结果文件的完整路径列表
     */
    public List<String> find(String[] args) {
        String order = GenSingleCallGraph.checkOrder();
        if (order == null) {
            return null;
        }

        if (args.length == 0) {
            // 当未指定参数时，先生成对应的方法完整调用链，再对生成目录的文件根据关键字生成到起始方法的调用链
            String[] args2 = prepare(order);
            if (args2 != null) {
                return doFind(args2);
            }
            return null;
        }

        // 当有指定参数时则直接使用
        return doFind(args);
    }

    // 设置处理目录时，需要返回生成文件路径列表
    public static void setReturnResultFileList() {
        System.setProperty(RETURN_RESULT_FILE_LIST_KEY, "1");
    }

    /**
     * 读取关键字配置，并生成对应的方法完整调用链
     *
     * @param order
     * @return null: 执行失败，非null: 执行成功，第0个元素为刚生成的保存方法完整调用链的目录，其他参数为配置文件中的关键字
     */
    private String[] prepare(String order) {
        boolean order4ee = GenSingleCallGraph.ORDER_FOR_EE.equals(order);

        // 读取指定的关键字
        String keywordConfigFilePath = JACGConstants.DIR_KEYWORD_CONF + File.separator + (order4ee ? JACGConstants.FILE_FIND_KEYWORD_4CALLEE : JACGConstants.FILE_FIND_KEYWORD_4CALLER);

        List<String> keywordList = FileUtil.readFile2List(keywordConfigFilePath);
        if (CommonUtil.isCollectionEmpty(keywordList)) {
            logger.error("请在配置文件中指定需要生成到起始方法之间调用链的关键字 {}", keywordConfigFilePath);
            return null;
        }

        int keywordNum = 0;
        for (String keyword : keywordList) {
            if (StringUtils.isNotBlank(keyword) && !keyword.startsWith(JACGConstants.FLAG_HASHTAG)) {
                keywordNum++;
            }
        }
        if (keywordNum == 0) {
            logger.error("请在配置文件中指定需要生成到起始方法之间调用链的合法关键字 {}", keywordConfigFilePath);
            return null;
        }

        // 生成方法完整调用链
        AbstractRunnerGenCallGraph runnerGenCallGraph;

        if (order4ee) {
            runnerGenCallGraph = new RunnerGenAllGraph4Callee();
        } else {
            runnerGenCallGraph = new RunnerGenAllGraph4Caller();
        }

        boolean success = runnerGenCallGraph.run();
        String outputPath = runnerGenCallGraph.getSuccessOutputDir();

        if (!success || outputPath == null) {
            logger.error("生成方法完整调用链失败，请检查");
            return null;
        }

        // 生成指定格式的数组
        String[] args = new String[keywordNum + 1];
        args[0] = outputPath;

        int arrayIndex = 0;
        for (String keyword : keywordList) {
            if (StringUtils.isNotBlank(keyword) && !keyword.startsWith(JACGConstants.FLAG_HASHTAG)) {
                args[++arrayIndex] = keyword;
            }
        }

        return args;
    }

    private List<String> doFind(String[] args) {
        int argsLength = args.length;
        if (argsLength < 2) {
            logger.error("参数数量小于2: {}", argsLength);
            logger.error("应按照以下方式指定参数: [文件/目录路径] [关键字1] [关键字2] ... [关键字n]");
            return null;
        }

        String filePath = args[0].replace("/", File.separator).replace("\\", File.separator);
        if (!filePath.endsWith(File.separator)) {
            filePath += File.separator;
        }

        Set<String> keywordSet = new TreeSet<>();
        for (int i = 1; i < argsLength; i++) {
            keywordSet.add(args[i]);
        }

        File file = new File(filePath);
        if (!file.exists()) {
            logger.error("文件或目录不存，请确认路径中是否存在空格，若是则需要使用双引号\"\"将路径包含: {}", filePath);
            return null;
        }

        if (!file.isFile()) {
            // 处理目录
            return handleDir(filePath, keywordSet);
        }

        // 处理文件
        String data = handleOneFile(filePath, keywordSet);
        if (data != null) {
            String headerInfo = GenSingleCallGraph.genHeaderInfo(filePath, keywordSet);
            // 指定文件时将结果直接输出到STDOUT
            System.out.println(headerInfo);
            System.out.println(data);
        }
        return null;
    }

    private List<String> handleDir(String dirPath, Set<String> keywordSet) {
        Set<String> subDirPathSet = new HashSet<>();
        List<String> subFilePathList = new ArrayList<>();

        searchDir(dirPath, subDirPathSet, subFilePathList);

        if (subFilePathList.isEmpty()) {
            logger.error("{} 目录中未找到后缀为[{}]的文件", dirPath, JACGConstants.EXT_TXT);
            return null;
        }

        int dirPathLength = dirPath.length();
        String currentDirPath = JACGConstants.DIR_FIND_KEYWORD_ + CommonUtil.currentTime();

        for (String subDirPath : subDirPathSet) {
            String newDirPath = dirPath + currentDirPath + File.separator + subDirPath.substring(dirPathLength);
            if (!FileUtil.isDirectoryExists(newDirPath)) {
                return null;
            }
        }

        for (String subFilePath : subFilePathList) {
            logger.info("处理文件: {}", subFilePath);
            String data = handleOneFile(subFilePath, keywordSet);
            // 生成.md文件，将.txt后缀去掉
            int lastPointIndex = subFilePath.lastIndexOf(JACGConstants.FLAG_DOT);
            String newFilePath = dirPath + currentDirPath + File.separator + subFilePath.substring(dirPathLength, lastPointIndex) + JACGConstants.EXT_MD;
            if (data != null) {
                String headerInfo = GenSingleCallGraph.genHeaderInfo(subFilePath, keywordSet);
                try (BufferedWriter out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(newFilePath), StandardCharsets.UTF_8))) {
                    out.write(headerInfo);
                    out.write(JACGConstants.NEW_LINE);
                    out.write(data);
                } catch (Exception e) {
                    logger.error("error ", e);
                    return null;
                }
            }
        }

        if (System.getProperty(RETURN_RESULT_FILE_LIST_KEY) == null) {
            return null;
        }

        // 返回生成的结果文件路径列表
        return getResultFileList(dirPath + currentDirPath);
    }

    private void searchDir(String dirPath, Set<String> subDirPathSet, List<String> subFilePathList) {
        File dir = new File(dirPath);
        File[] files = dir.listFiles();
        for (File file : files) {
            if (file.isDirectory()) {
                searchDir(file.getPath(), subDirPathSet, subFilePathList);
            } else {
                String filePath = file.getPath();
                if (filePath.endsWith(JACGConstants.EXT_TXT)) {
                    subDirPathSet.add(dirPath);
                    subFilePathList.add(filePath);
                }
            }
        }
    }

    private String handleOneFile(String filePath, Set<String> keywordSet) {
        List<String> lineNumList = findKeywordLineNumList(filePath, keywordSet);

        if (lineNumList == null || lineNumList.size() == 1) {
            logger.error("{} 未查找到指定关键字: {}", filePath, keywordSet);
            return null;
        }

        GenSingleCallGraph genSingleCallGraph = new GenSingleCallGraph();
        return genSingleCallGraph.genCallGraph(lineNumList.toArray(new String[]{}));
    }

    private List<String> findKeywordLineNumList(String file, Set<String> keywordSet) {
        List<String> lineNumList = new ArrayList<>(100);
        lineNumList.add(file);

        int index = 1;
        try (BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream(file), StandardCharsets.UTF_8))) {
            String line;
            while ((line = in.readLine()) != null) {
                for (String keyword : keywordSet) {
                    if (line.contains(keyword)) {
                        lineNumList.add(String.valueOf(index));
                    }
                }
                index++;
            }

            return lineNumList;
        } catch (Exception e) {
            logger.error("error ", e);
            return null;
        }
    }

    private List<String> getResultFileList(String dirPath) {
        File dir = new File(dirPath);
        File[] files = dir.listFiles();

        List<String> resultFileList = new ArrayList<>(files.length);

        for (File file : files) {
            if (file.isFile() && file.getName().endsWith(JACGConstants.EXT_MD)) {
                if (file.getName().startsWith(JACGConstants.COMBINE_FILE_NAME_PREFIX)) {
                    // 跳过合并文件
                    continue;
                }

                resultFileList.add(file.getAbsolutePath());
            }
        }

        return resultFileList;
    }
}
