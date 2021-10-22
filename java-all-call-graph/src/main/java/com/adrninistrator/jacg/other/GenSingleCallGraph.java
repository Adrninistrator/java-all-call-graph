package com.adrninistrator.jacg.other;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.util.FileUtil;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2021/7/2
 * @description:
 */

public class GenSingleCallGraph {

    public static final String ORDER_KEY = "order";
    public static final String ORDER_FOR_ER = "4er";
    public static final String ORDER_FOR_EE = "4ee";

    public String genCallGraph(String[] args) {
        String order = checkOrder();
        if (order == null) {
            return null;
        }

        if (!check(args)) {
            System.err.println("应按照以下方式指定参数: [文件路径] [行号1] [行号2] ... [行号n]");
            return null;
        }

        String filePath = args[0];

        StringBuilder stringBuilder = new StringBuilder();

        for (int i = 1; i < args.length; i++) {
            String strLineNum = args[i];
            if (!isValidNum(strLineNum)) {
                System.err.println(filePath + " 第" + i + "个行号非法: " + strLineNum);
                continue;
            }

            int lineNum = Integer.parseInt(strLineNum);
            if (lineNum < 2) {
                System.err.println(filePath + " 第" + i + "个行号过小: " + strLineNum);
                continue;
            }

            print(filePath, lineNum, ORDER_FOR_ER.equals(order), stringBuilder);
        }

        return stringBuilder.toString();
    }

    public static String genHeaderInfo(String filePath, Set<String> keywordSet) {
        StringBuilder stringBuilder = new StringBuilder();
        stringBuilder.append("- 处理文件: ").append(filePath).append(JACGConstants.NEW_LINE);

        String order = System.getProperty(ORDER_KEY);
        boolean order4er = ORDER_FOR_ER.equals(order);
        if (order4er) {
            stringBuilder.append("- 查看方法向下调用链时使用，按层级增大方向打印").append(JACGConstants.NEW_LINE);
        } else {
            stringBuilder.append("- 查看方法向上调用链时使用，按层级减小方向打印").append(JACGConstants.NEW_LINE);
        }
        if (keywordSet != null) {
            stringBuilder.append("- 查找关键字: ").append(JACGConstants.NEW_LINE).append("```").append(JACGConstants.NEW_LINE);
            for (String keyword : keywordSet) {
                stringBuilder.append(keyword).append(JACGConstants.NEW_LINE);
            }
            stringBuilder.append("```").append(JACGConstants.NEW_LINE);
        }
        return stringBuilder.toString();
    }

    public static String checkOrder() {
        String order = System.getProperty(ORDER_KEY);
        if (!ORDER_FOR_ER.equals(order) && !ORDER_FOR_EE.equals(order)) {
            System.err.println("请通过-D" + ORDER_KEY + "=" + ORDER_FOR_ER + " 或 -D" + ORDER_KEY + "=" + ORDER_FOR_EE +
                    " 指定打印顺序，b代表按层级增大方向，s代表按层级减小方向");
            return null;
        }

        return order;
    }

    private boolean check(String[] args) {
        if (args.length < 2) {
            System.err.println("参数数量太少: " + args.length);
            return false;
        }

        String filePath = args[0];
        if (!FileUtil.isFileExists(filePath)) {
            System.err.println("文件不存在或不是文件，请确认文件路径中是否存在空格，若是则需要使用双引号\"\"将文件路径包含: " + filePath);
            return false;
        }

        return true;
    }

    private void print(String file, int lineNum, boolean order4er, StringBuilder stringBuilder) {

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
            System.err.println(file + " 文件指定行未找到方法调用级别: " + lineNum);
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

        StringBuilder tmpStr = new StringBuilder();
        if (order4er) {
            for (int i = resultList.size() - 1; i >= 0; i--) {
                tmpStr.append(resultList.get(i)).append(JACGConstants.NEW_LINE);
            }
        } else {
            for (String str : resultList) {
                tmpStr.append(str).append(JACGConstants.NEW_LINE);
            }
        }

        String data = tmpStr.toString();
        stringBuilder.append("# 行号: ").append(lineNum).append(JACGConstants.NEW_LINE)
                .append("```").append(JACGConstants.NEW_LINE)
                .append(data).append("```").append(JACGConstants.NEW_LINE).append(JACGConstants.NEW_LINE);
    }

    private boolean isValidNum(String str) {
        if (str == null || str.isEmpty()) {
            return false;
        }

        for (char ch : str.toCharArray()) {
            if (ch > '9' || ch < '0') {
                return false;
            }
        }
        return true;
    }

    private Integer getMethodLevel(String line) {
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

    // 设置生成的调用链顺序为向下
    public static void setOrder4er() {
        System.setProperty(ORDER_KEY, ORDER_FOR_ER);
    }

    // 设置生成的调用链顺序为向上
    public static void setOrder4ee() {
        System.setProperty(ORDER_KEY, ORDER_FOR_EE);
    }
}
