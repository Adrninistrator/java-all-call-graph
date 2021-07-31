package com.adrninistrator.jacg.other;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2021/7/29
 * @description:
 */

public class FindKeywordCallGraph {

    public static void main(String[] args) {
        String order = GenSingleCallGraph.checkOrder();
        if (order == null) {
            return;
        }

        FindKeywordCallGraph findKeyWordCallGraph = new FindKeywordCallGraph();
        if (!findKeyWordCallGraph.check(args)) {
            System.out.println("应按照以下方式指定参数：[文件路径] [关键字]");
            return;
        }

        List<String> lineNumList = findKeyWordCallGraph.findKeywordLineNumList(args[0], args[1]);
        if (lineNumList == null) {
            return;
        }

        if (lineNumList.size() == 1) {
            System.err.println("未查找到指定关键字: " + args[1]);
            return;
        }

        GenSingleCallGraph.main(lineNumList.toArray(new String[]{}));
    }

    private boolean check(String[] args) {
        if (args.length != 2) {
            System.err.println("参数数量不是2: " + args.length);
            return false;
        }

        String filePath = args[0];
        File file = new File(filePath);
        if (!file.exists() || !file.isFile()) {
            System.err.println("文件不存在或不是文件，请确认文件路径中是否存在空格，若是则需要使用双引号\"\"将文件路径包含: " + filePath);
            return false;
        }

        return true;
    }

    private List<String> findKeywordLineNumList(String file, String keyword) {
        List<String> lineNumList = new ArrayList<>(100);
        lineNumList.add(file);

        int index = 1;
        try (BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream(file), StandardCharsets.UTF_8))) {
            String line;
            while ((line = in.readLine()) != null) {
                if (line.contains(keyword)) {
                    lineNumList.add(String.valueOf(index));
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
