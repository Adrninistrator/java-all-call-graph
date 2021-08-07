package com.adrninistrator.jacg.other;

import com.adrninistrator.jacg.common.Constants;
import com.adrninistrator.jacg.util.CommonUtil;
import com.adrninistrator.jacg.util.FileUtilNoLogger;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.*;

/**
 * @author adrninistrator
 * @date 2021/7/29
 * @description:
 */

public class FindKeywordCallGraph {

    public void find(String[] args) {
        String order = GenSingleCallGraph.checkOrder();
        if (order == null) {
            return;
        }

        int argsLength = args.length;
        if (argsLength < 2) {
            System.err.println("参数数量小于2: " + argsLength);
            System.err.println("应按照以下方式指定参数: [文件/目录路径] [关键字1] [关键字2] ... [关键字n]");
            return;
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
            System.err.println("文件或目录不存，请确认路径中是否存在空格，若是则需要使用双引号\"\"将路径包含: " + filePath);
            return;
        }

        if (file.isFile()) {
            String data = handleOneFile(filePath, keywordSet);
            if (data != null) {
                String headerInfo = GenSingleCallGraph.genHeaderInfo(filePath, keywordSet);
                System.out.println(headerInfo);
                System.out.println(data);
            }
        } else {
            handleDir(filePath, keywordSet);
        }
    }

    private void handleDir(String dirPath, Set<String> keywordSet) {
        Set<String> subDirPathSet = new HashSet<>();
        List<String> subFilePathList = new ArrayList<>();

        searchDir(dirPath, subDirPathSet, subFilePathList);

        if (subFilePathList.isEmpty()) {
            System.err.println(dirPath + " 目录中未找到后缀为[" + Constants.EXT_TXT + "]的文件");
            return;
        }

        int dirPathLength = dirPath.length();
        String currentDirPathFlag = Constants.DIR_FIND_KEYWORD_ + CommonUtil.currentTime();

        for (String subDirPath : subDirPathSet) {
            String newDirPath = dirPath + currentDirPathFlag + File.separator + subDirPath.substring(dirPathLength);
            if (!FileUtilNoLogger.isDirectoryExists(newDirPath, true)) {
                return;
            }
        }
        for (String subFilePath : subFilePathList) {
            System.out.println("处理文件: " + subFilePath);
            String data = handleOneFile(subFilePath, keywordSet);
            String newFilePath = dirPath + currentDirPathFlag + File.separator + subFilePath.substring(dirPathLength) + Constants.EXT_MD;
            if (data != null) {
                String headerInfo = GenSingleCallGraph.genHeaderInfo(subFilePath, keywordSet);
                try (BufferedWriter out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(newFilePath), StandardCharsets.UTF_8))) {
                    out.write(headerInfo);
                    out.write(Constants.NEW_LINE);
                    out.write(data);
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        }
    }

    private void searchDir(String dirPath, Set<String> subDirPathSet, List<String> subFilePathList) {
        File dir = new File(dirPath);
        File[] files = dir.listFiles();
        for (File file : files) {
            if (file.isDirectory()) {
                searchDir(file.getPath(), subDirPathSet, subFilePathList);
            } else {
                String filePath = file.getPath();
                if (filePath.endsWith(Constants.EXT_TXT)) {
                    subDirPathSet.add(dirPath);
                    subFilePathList.add(filePath);
                }
            }
        }
    }

    private String handleOneFile(String filePath, Set<String> keywordSet) {
        List<String> lineNumList = findKeywordLineNumList(filePath, keywordSet);

        if (lineNumList.size() == 1) {
            System.err.println(filePath + " 未查找到指定关键字: " + keywordSet);
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
            e.printStackTrace();
            return null;
        }
    }
}
