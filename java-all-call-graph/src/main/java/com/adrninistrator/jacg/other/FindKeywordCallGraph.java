package com.adrninistrator.jacg.other;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.extensions.find_filter.BaseFindKeywordFilter;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Callee;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Caller;
import com.adrninistrator.jacg.runner.base.AbstractRunnerGenCallGraph;
import com.adrninistrator.jacg.util.FileUtil;
import com.adrninistrator.jacg.util.JACGUtil;
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

    public static final String GEN_FOR_CALLER_SUPPORT_IGNORE_KEY = "gen_for_caller_support_ignore";

    public static final String NO_RESULT = "未查找到指定关键字";

    // 记录当前处理的目录
    private String currentDirPath;

    // 处理目录时，需要返回生成文件路径列表
    private boolean returnResultFileListKey = false;

    // 关键字搜索自定义过滤处理类
    private BaseFindKeywordFilter baseFindKeywordFilter;

    // 额外指定的关键字
    private List<String> extraKeywordList = new ArrayList<>();

    // 生成目录名使用固定值，不指定时间
    private boolean outputDirUseFixedValue = false;

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

    // 返回当前处理的目录
    public String getCurrentDirPath() {
        return currentDirPath;
    }

    public void setBaseFindKeywordFilter(BaseFindKeywordFilter baseFindKeywordFilter) {
        this.baseFindKeywordFilter = baseFindKeywordFilter;
    }

    public void addExtraKeyword(String extraKeyword) {
        extraKeywordList.add(extraKeyword);
    }

    // 设置处理目录时，需要返回生成文件路径列表
    public void setReturnResultFileList() {
        returnResultFileListKey = true;
    }

    // 设置生成向下的完整方法调用链时，支持忽略特定的包名、类、方法
    public static void setGenForCallerSupportIgnore() {
        System.setProperty(GEN_FOR_CALLER_SUPPORT_IGNORE_KEY, "1");
    }

    // 设置生成目录名使用固定值，不指定时间
    public void setOutputDirUseFixedValue(boolean outputDirUseFixedValue) {
        this.outputDirUseFixedValue = outputDirUseFixedValue;
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
        OtherConfigFileUseListEnum otherConfigFileUseListEnum = order4ee ? OtherConfigFileUseListEnum.OCFULE_FIND_KEYWORD_4CALLEE :
                OtherConfigFileUseListEnum.OCFULE_FIND_KEYWORD_4CALLER;
        String keywordConfigFilePath = otherConfigFileUseListEnum.getFileName();

        List<String> keywordList = ConfigureWrapper.getOtherConfigList(otherConfigFileUseListEnum);
        if (JACGUtil.isCollectionEmpty(keywordList)) {
            logger.error("请在配置文件中指定需要生成到起始方法之间调用链的关键字 {}", keywordConfigFilePath);
            return null;
        }

        // 添加额外关键字
        for (String extraKeyword : extraKeywordList) {
            if (!keywordList.contains(extraKeyword)) {
                keywordList.add(extraKeyword);
            }
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

            if (System.getProperty(GEN_FOR_CALLER_SUPPORT_IGNORE_KEY) != null) {
                ((RunnerGenAllGraph4Caller) runnerGenCallGraph).setSupportIgnore(true);
            }
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

        Set<String> keywordSet = new TreeSet<>(Arrays.asList(args).subList(1, argsLength));

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
            // 指定处理文件时将结果直接输出到STDOUT
            System.out.println(headerInfo);
            System.out.println(data);
        }
        return null;
    }

    // 处理目录
    private List<String> handleDir(String dirPath, Set<String> keywordSet) {
        Set<String> subDirPathSet = new HashSet<>();
        List<String> subFilePathList = new ArrayList<>();

        // 从目录中查找需要处理的文件
        searchDir(dirPath, subDirPathSet, subFilePathList);

        if (subFilePathList.isEmpty()) {
            logger.error("{} 目录中未找到后缀为[{}]的文件", dirPath, JACGConstants.EXT_TXT);
            return null;
        }

        int dirPathLength = dirPath.length();
        // 记录当前处理的目录
        currentDirPath = dirPath + JACGConstants.DIR_FIND_KEYWORD_;
        if (!outputDirUseFixedValue) {
            currentDirPath = currentDirPath + "_" + JACGUtil.currentTime();
        }

        for (String subDirPath : subDirPathSet) {
            String newDirPath = currentDirPath + File.separator + subDirPath.substring(dirPathLength);
            if (!FileUtil.isDirectoryExists(newDirPath)) {
                return null;
            }
        }

        for (String subFilePath : subFilePathList) {
            logger.info("处理文件: {}", subFilePath);
            String data = handleOneFile(subFilePath, keywordSet);
            // 生成.md文件，将.txt后缀去掉
            int lastPointIndex = subFilePath.lastIndexOf(JACGConstants.FLAG_DOT);
            String newFilePath = currentDirPath + File.separator + subFilePath.substring(dirPathLength, lastPointIndex) + JACGConstants.EXT_MD;
            if (data != null) {
                String headerInfo = GenSingleCallGraph.genHeaderInfo(subFilePath, keywordSet);
                try (BufferedWriter out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(newFilePath), StandardCharsets.UTF_8))) {
                    if (!NO_RESULT.equals(data)) {
                        // 查找到结果时写入文件，未查找到时创建空文件
                        out.write(headerInfo + JACGConstants.NEW_LINE + data);
                    }
                } catch (Exception e) {
                    logger.error("error ", e);
                    return null;
                }
            }
        }

        if (!returnResultFileListKey) {
            return null;
        }

        List<String> resultFileList = new ArrayList<>();

        // 返回生成的结果文件路径列表
        getResultFileList(currentDirPath, resultFileList);

        return resultFileList;
    }

    // 从目录中查找需要处理的文件
    private void searchDir(String dirPath, Set<String> subDirPathSet, List<String> subFilePathList) {
        File dir = new File(dirPath);
        File[] files = dir.listFiles();
        if (files == null) {
            return;
        }

        for (File file : files) {
            if (file.isDirectory()) {
                // 目录，递归
                searchDir(file.getPath(), subDirPathSet, subFilePathList);
            } else {
                // 文件
                String filePath = file.getPath();
                if (filePath.endsWith(JACGConstants.EXT_TXT) &&
                        !StringUtils.startsWithAny(file.getName(), JACGConstants.COMBINE_FILE_NAME_PREFIX, JACGConstants.FILE_MAPPING_NAME)) {
                    // 跳过合并文件，跳过映射文件
                    subDirPathSet.add(dirPath);
                    subFilePathList.add(filePath);
                }
            }
        }
    }

    private String handleOneFile(String filePath, Set<String> keywordSet) {
        // 获得包含关键字的文件行号
        List<String> lineNumList = findKeywordLineNumList(filePath, keywordSet);

        if (lineNumList == null || lineNumList.size() == 1) {
            logger.warn("{} 未查找到指定关键字: {}", filePath, keywordSet);
            return NO_RESULT;
        }

        GenSingleCallGraph genSingleCallGraph = new GenSingleCallGraph();
        return genSingleCallGraph.genCallGraph(lineNumList.toArray(new String[]{}));
    }

    // 获得包含关键字的文件行号
    private List<String> findKeywordLineNumList(String file, Set<String> keywordSet) {
        List<String> lineNumList = new ArrayList<>(100);
        lineNumList.add(file);

        int index = 1;
        try (BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream(file), StandardCharsets.UTF_8))) {
            String line;
            while ((line = in.readLine()) != null) {
                for (String keyword : keywordSet) {
                    // 根据关键字判断当前行是否需要显示
                    if (line.contains(keyword) &&
                            (baseFindKeywordFilter == null || baseFindKeywordFilter.filter(keyword, line))) {
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

    private void getResultFileList(String dirPath, List<String> resultFileList) {
        File dir = new File(dirPath);
        File[] files = dir.listFiles();
        if (files == null) {
            return;
        }

        for (File file : files) {
            if (file.isDirectory()) {
                getResultFileList(file.getPath(), resultFileList);
            } else if (file.getName().endsWith(JACGConstants.EXT_MD)) {
                if (file.getName().startsWith(JACGConstants.COMBINE_FILE_NAME_PREFIX)) {
                    // 跳过合并文件
                    continue;
                }

                resultFileList.add(file.getAbsolutePath());
            }
        }
    }
}
