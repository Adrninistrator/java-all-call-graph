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
 * @date 2021/7/2
 * @description:
 */

public class GenSingleCallGraph {

    public static void main(String[] args) {
        if (args.length != 2) {
            System.err.println("参数数量不是2: " + args.length);
            return;
        }

        String filePath = args[0];
        File file = new File(filePath);
        if (!file.exists() || !file.isFile()) {
            System.err.println("文件不存在或不是文件: " + filePath);
            return;
        }

        String strLineNum = args[1];
        if (!isValidNum(strLineNum)) {
            System.err.println("文件行数非法: " + strLineNum);
            return;
        }

        int lineNum = Integer.parseInt(strLineNum);
        if (lineNum < 2) {
            System.err.println("文件行数过小: " + strLineNum);
            return;
        }

        List<String> dataList = new ArrayList<>(lineNum);

        int readLine = 0;
        try (BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream(file), StandardCharsets.UTF_8))) {
            while (true) {
                String line = in.readLine();
                dataList.add(line);

                readLine++;
                if (readLine >= lineNum) {
                    break;
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }

        String calledMethod = dataList.get(lineNum - 1);

        Integer startLevel = getMethodLevel(calledMethod);
        if (startLevel == null) {
            System.err.println("文件指定行未找到方法调用级别: " + strLineNum);
            return;
        }

        int currentLevel = startLevel.intValue();

        List<String> resultList = new ArrayList<>(currentLevel + 1);
        resultList.add(calledMethod);

        for (int i = dataList.size() - 2; i >= 0; i--) {
            String tmpData = dataList.get(i);
            Integer tmpLevel = getMethodLevel(tmpData);
            if (tmpLevel == null) {
                System.err.println("当前行数据非法: " + tmpData);
                return;
            }
            if (tmpLevel.intValue() == currentLevel - 1) {
                resultList.add(tmpData);
                currentLevel--;
                if (currentLevel == 0) {
                    break;
                }
            }
        }

        System.out.println("顺序调用结果：");
        for (String str : resultList) {
            System.out.println(str);
        }

        System.out.println("\n逆序调用结果：");
        for (int i = resultList.size() - 1; i >= 0; i--) {
            System.out.println(resultList.get(i));
        }
    }

    private static boolean isValidNum(String str) {
        for (char ch : str.toCharArray()) {
            if (ch > '9' || ch < '0') {
                return false;
            }
        }
        return true;
    }

    private static Integer getMethodLevel(String line) {
        int index1 = line.indexOf('[');
        if (index1 == -1) {
            System.err.println("未找到[: " + line);
            return null;
        }

        int index2 = line.indexOf(']');
        if (index2 == -1) {
            System.err.println("未找到]: " + line);
            return null;
        }

        String strLevel = line.substring(index1 + 1, index2);
        if (!isValidNum(strLevel)) {
            return null;
        }

        return Integer.valueOf(strLevel);
    }
}
