package com.adrninistrator.jacg.runner;

import com.adrninistrator.jacg.common.Constants;
import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.dto.TmpNode4Callee;
import com.adrninistrator.jacg.runner.base.AbstractRunnerGenCallGraph;
import com.adrninistrator.jacg.util.CommonUtil;
import com.adrninistrator.jacg.util.FileUtil;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.MutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2021/6/20
 * @description: 从数据库读取数据，生成调用指定类的所有向上的调用关系
 */

public class RunnerGenAllGraph4Callee extends AbstractRunnerGenCallGraph {

    private static final Logger logger = LoggerFactory.getLogger(RunnerGenAllGraph4Callee.class);

    static {
        runner = new RunnerGenAllGraph4Callee();
    }

    @Override
    public boolean init() {
        String taskInfoFile = Constants.DIR_CONFIG + File.separator + Constants.FILE_OUT_GRAPH_FOR_CALLEE_CLASS_NAME;
        // 读取配置文件中指定的需要处理的任务
        if (!readTaskInfo(taskInfoFile)) {
            return false;
        }

        // 创建输出文件所在目录
        if (!createOutputDit(Constants.DIR_OUTPUT_GRAPH_FOR_CALLEE)) {
            return false;
        }

        if (confInfo.isGenUpwardsMethodsFile()) {
            String dirPath = outputDirPrefix + File.separator + Constants.DIR_METHODS;
            if (!FileUtil.isDirectoryExists(dirPath)) {
                return false;
            }
        }

        return true;
    }

    @Override
    public void operate() {
        // 读取方法注解
        if (confInfo.isShowMethodAnnotation() && !readMethodAnnotation()) {
            return;
        }

        // 创建线程
        createThreadPoolExecutor();

        // 遍历需要处理的任务
        for (String task : taskSet) {

            // 等待直到允许任务执行
            wait4TPEExecute();

            threadPoolExecutor.execute(() -> {
                // 处理一条记录
                if (!handleOneRecord(task)) {
                    someTaskFail = true;
                }
            });
        }

        // 等待直到任务执行完毕
        wait4TPEDone();

        // 将输出文件合并
        combineOutputFile(Constants.COMBINE_FILE_NAME_4_CALLEE);
    }

    // 处理一条记录
    private boolean handleOneRecord(String calleeClassName) {
        // 从方法调用关系表查询指定的类是否存在
        if (!checkClassNameExists(calleeClassName)) {
            return false;
        }

        // 查找指定被调用类的全部方法
        String sql = sqlCacheMap.get(Constants.SQL_KEY_MC_QUERY_CALLEE_ALL_METHODS);
        if (sql == null) {
            sql = "select distinct(" + DC.MC_CALLEE_METHOD_HASH + ")," + DC.MC_CALLEE_FULL_METHOD + " from " +
                    Constants.TABLE_PREFIX_METHOD_CALL + confInfo.getAppName() + " where " + DC.MC_CALLEE_CLASS_NAME +
                    "= ? order by " + DC.MC_CALLEE_METHOD_HASH;
            cacheSql(Constants.SQL_KEY_MC_QUERY_CALLEE_ALL_METHODS, sql);
        }

        List<Map<String, Object>> calleeMethodList = dbOperator.queryList(sql, new Object[]{calleeClassName});
        if (CommonUtil.isCollectionEmpty(calleeMethodList)) {
            logger.error("从方法调用关系表未找到被调用类对应方法 {} {}", calleeClassName);
            return false;
        }

        // 确定当前类对应输出文件名，格式：配置文件中指定的类名.txt
        String outputFile4ClassName = outputDirPrefix + File.separator + calleeClassName + Constants.EXT_TXT;
        logger.info("当前类输出文件名 {}", outputFile4ClassName);

        try (BufferedWriter out4Class = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(outputFile4ClassName),
                StandardCharsets.UTF_8))) {
            for (Map<String, Object> calleeMethodMap : calleeMethodList) {
                String calleeMethodHash = (String) calleeMethodMap.get(DC.MC_CALLEE_METHOD_HASH);
                String calleeFullMethod = (String) calleeMethodMap.get(DC.MC_CALLEE_FULL_METHOD);

                // 处理一个被调用方法
                if (confInfo.isGenUpwardsMethodsFile()) {
                    String methodName = CommonUtil.getOnlyMethodName(calleeFullMethod);
                    String outputFile4Method = outputDirPrefix + File.separator + Constants.DIR_METHODS + File.separator + calleeClassName +
                            Constants.FLAG_AT + methodName + Constants.FLAG_AT + calleeMethodHash + Constants.EXT_TXT;
                    logger.info("当前方法输出文件名 {}", outputFile4Method);
                    BufferedWriter out4Method = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(outputFile4Method),
                            StandardCharsets.UTF_8));

                    handleOneCalleeMethod(calleeClassName, calleeMethodHash, calleeFullMethod, out4Class, out4Method);

                    IOUtils.close(out4Method);
                } else {
                    handleOneCalleeMethod(calleeClassName, calleeMethodHash, calleeFullMethod, out4Class, null);
                }

                // 每个方法信息间插入一条空行
                out4Class.write(Constants.NEW_LINE);
            }
            return true;
        } catch (Exception e) {
            logger.error("error ", e);
            return false;
        }
    }

    // 处理一个被调用方法
    private boolean handleOneCalleeMethod(String calleeClassName, String calleeMethodHash, String calleeFullMethod, BufferedWriter out4Class,
                                          BufferedWriter out4Method) throws IOException {
        // 在文件第1行写入当前方法的完整信息
        writeData2File(calleeFullMethod, out4Class, out4Method);
        writeData2File(Constants.NEW_LINE, out4Class, out4Method);

        // 确定写入输出文件的当前调用方法信息
        String callerInfo = chooseCallerInfo(calleeClassName, calleeFullMethod);

        // 第2行写入当前方法的信息
        writeData2File(genOutputPrefix(0), out4Class, out4Method);
        writeData2File(callerInfo, out4Class, out4Method);
        // 写入方法注解
        String methodAnnotation = methodAnnotationsMap.get(calleeMethodHash);
        if (methodAnnotation != null) {
            writeData2File(methodAnnotation, out4Class, out4Method);
        }
        writeData2File(Constants.NEW_LINE, out4Class, out4Method);

        // 记录查找到的调用方法信息List
        List<Pair<String, Boolean>> callerMethodList = new ArrayList<>(Constants.BATCH_SIZE);

        // 根据指定的调用者方法HASH，查找所有被调用的方法信息
        if (!genAllGraph4Callee(calleeMethodHash, callerMethodList, calleeFullMethod)) {
            return false;
        }

        // 记录所有的调用方法
        for (Pair<String, Boolean> pair : callerMethodList) {
            writeData2File(pair.getLeft(), out4Class, out4Method);
            if (pair.getRight().booleanValue()) {
                // 对于入口方法，写入标志
                writeData2File(Constants.CALLEE_FLAG_ENTRY, out4Class, out4Method);
            }
            writeData2File(Constants.NEW_LINE, out4Class, out4Method);
        }

        return true;
    }

    private void writeData2File(String data, BufferedWriter out4Class, BufferedWriter out4Method) throws IOException {
        out4Class.write(data);
        if (out4Method != null) {
            out4Method.write(data);
        }
    }

    /**
     * 根据指定的被调用者方法HASH，查找所有调用方法信息
     *
     * @param calleeMethodHash
     * @param callerMethodList
     * @param calleeFullMethod
     * @return
     */
    protected boolean genAllGraph4Callee(String calleeMethodHash, List<Pair<String, Boolean>> callerMethodList, String calleeFullMethod) {

        // 通过List记录当前遍历到的节点信息（当作不定长数组使用）
        List<TmpNode4Callee> node4CalleeList = new ArrayList<>();

        // 初始加入最下层节点，callerMethodHash设为null
        TmpNode4Callee headNode = TmpNode4Callee.genNode(calleeMethodHash, null);
        node4CalleeList.add(headNode);

        // 记录当前处理的节点层级
        int currentNodeLevel = 0;

        int lineNum = 0;

        while (true) {
            TmpNode4Callee currentNode = node4CalleeList.get(currentNodeLevel);

            // 查询当前节点的一个上层调用方法
            Map<String, Object> methodMapByCallee = queryOneByCalleeMethod(currentNode);
            if (methodMapByCallee == null) {
                // 查询失败
                return false;
            }

            if (methodMapByCallee.isEmpty()) {
                // 未查询到记录
                if (currentNodeLevel <= 0) {
                    // 当前处理的节点为最下层节点，结束循环
                    // 将调用方法列表中最后一条记录设置为入口方法
                    markMethodAsEntry(callerMethodList);
                    return true;
                }

                // 当前处理的节点不是最下层节点，返回下一层处理
                currentNodeLevel--;

                // 将调用方法列表中最后一条记录设置为入口方法
                markMethodAsEntry(callerMethodList);
                continue;
            }

            // 查询到记录
            lineNum++;
            if (lineNum % Constants.NOTICE_LINE_NUM == 0) {
                logger.info("记录数达到 {} {}", lineNum, calleeFullMethod);
            }

            String currentCallerMethodHash = (String) methodMapByCallee.get(DC.MC_CALLER_METHOD_HASH);

            // 检查是否出现循环调用
            int back2Level = checkCycleCall(node4CalleeList, currentNodeLevel, currentCallerMethodHash);

            // 记录调用方法信息
            recordCallerInfo(methodMapByCallee, currentNodeLevel, currentCallerMethodHash, back2Level, callerMethodList);

            if (back2Level != Constants.NO_CYCLE_CALL_FLAG) {
                logger.info("找到循环调用 {} [{}]", currentCallerMethodHash, back2Level);

                // 将当前处理的层级指定到循环调用的节点
                currentNodeLevel = back2Level;
                continue;
            }

            // 更新当前处理节点的callerMethodHash
            node4CalleeList.get((currentNodeLevel)).setCurrentCallerMethodHash(currentCallerMethodHash);

            // 继续上一层处理
            currentNodeLevel++;

            // 获取上一层节点
            if (currentNodeLevel + 1 > node4CalleeList.size()) {
                // 上一层节点不存在，则需要添加，callerMethodHash设为null
                TmpNode4Callee nextNode = TmpNode4Callee.genNode(currentCallerMethodHash, null);
                node4CalleeList.add(nextNode);
            } else {
                // 上一层节点已存在，则修改值
                TmpNode4Callee nextNode = node4CalleeList.get(currentNodeLevel);
                nextNode.setCurrentCalleeMethodHash(currentCallerMethodHash);
                nextNode.setCurrentCallerMethodHash(null);
            }
        }
    }

    /**
     * 检查是否出现循环调用
     *
     * @param node4CalleeList
     * @param currentNodeLevel        不需要遍历整个List，因为后面的元素可能当前无效
     * @param currentCallerMethodHash
     * @return -1: 未出现循环调用，非-1: 出现循环依赖，值为发生循环调用的层级
     */
    private int checkCycleCall(List<TmpNode4Callee> node4CalleeList, int currentNodeLevel, String currentCallerMethodHash) {
        /*
            应该根据当前找到的callerMethodHash，从列表中找到calleeMethodHash相同的节点，以这一层级作为被循环调用的层级

            node4CalleeList中的示例
            层级：   ee   er
            [0]:    a <- b
            [1]:    b <- c
            [2]:    c <- d
            [3]:    d <- a

            第3层级：er: a，根据ee为a，找到第0层级
         */
        for (int i = currentNodeLevel; i >= 0; i--) {
            if (currentCallerMethodHash.equals(node4CalleeList.get(i).getCurrentCalleeMethodHash())) {
                return i;
            }
        }
        return Constants.NO_CYCLE_CALL_FLAG;
    }

    // 将调用方法列表中最后一条记录设置为入口方法
    private void markMethodAsEntry(List<Pair<String, Boolean>> callerMethodList) {
        if (!CommonUtil.isCollectionEmpty(callerMethodList)) {
            Pair<String, Boolean> pair = callerMethodList.get(callerMethodList.size() - 1);
            pair.setValue(Boolean.TRUE);
        }
    }

    // 查询当前节点的一个上层调用方法
    private Map<String, Object> queryOneByCalleeMethod(TmpNode4Callee node) {
        // 确定通过调用方法进行查询使用的SQL语句
        String sql = chooseQueryByCalleeMethodSql(node.getCurrentCallerMethodHash());

        List<Map<String, Object>> list;
        if (node.getCurrentCallerMethodHash() == null) {
            list = dbOperator.queryList(sql, new Object[]{node.getCurrentCalleeMethodHash()});
        } else {
            list = dbOperator.queryList(sql, new Object[]{node.getCurrentCalleeMethodHash(), node.getCurrentCallerMethodHash()});
        }

        if (list == null) {
            // 查询失败
            return null;
        }

        if (CommonUtil.isCollectionEmpty(list)) {
            // 查询不到结果时，返回空Map
            return new HashMap<>(0);
        }

        return list.get(0);
    }

    // 确定通过调用方法进行查询使用的SQL语句
    protected String chooseQueryByCalleeMethodSql(String callerMethodHash) {
        if (callerMethodHash == null) {
            // 第一次查询
            String sql = sqlCacheMap.get(Constants.SQL_KEY_MC_QUERY_ONE_CALLER1);
            if (sql == null) {
                // 确定查询被调用关系时所需字段
                String callerColumns = chooseCallerColumns();
                sql = "select " + callerColumns + ", " + DC.MC_CALLER_METHOD_HASH + ", " + DC.MC_ID + " from " +
                        Constants.TABLE_PREFIX_METHOD_CALL + confInfo.getAppName() + " where " +
                        DC.MC_CALLEE_METHOD_HASH + " = ? order by " +
                        DC.MC_CALLEE_METHOD_HASH + ", " + DC.MC_CALLER_METHOD_HASH + " limit 1";
                cacheSql(Constants.SQL_KEY_MC_QUERY_ONE_CALLER1, sql);
            }
            return sql;
        }

        // 不是第一次查询
        String sql = sqlCacheMap.get(Constants.SQL_KEY_MC_QUERY_ONE_CALLER2);
        if (sql == null) {
            // 确定查询被调用关系时所需字段
            String callerColumns = chooseCallerColumns();

            // 确定查询被调用关系时所需字段
            sql = "select " + callerColumns + ", " + DC.MC_CALLER_METHOD_HASH + ", " + DC.MC_ID + " from " +
                    Constants.TABLE_PREFIX_METHOD_CALL + confInfo.getAppName() + " where " +
                    DC.MC_CALLEE_METHOD_HASH + " = ? and " + DC.MC_CALLER_METHOD_HASH + " > ? order by " +
                    DC.MC_CALLEE_METHOD_HASH + ", " + DC.MC_CALLER_METHOD_HASH + " limit 1";
            cacheSql(Constants.SQL_KEY_MC_QUERY_ONE_CALLER2, sql);
        }
        return sql;
    }

    // 记录调用方法信息
    protected void recordCallerInfo(Map<String, Object> callerMethodMap, int currentNodeLevel, String currentCallerMethodHash, int back2Level,
                                    List<Pair<String, Boolean>> callerMethodList) {

        String fullMethod = (String) callerMethodMap.get(DC.MC_CALLER_FULL_METHOD);

        StringBuilder callerInfo = new StringBuilder();
        callerInfo.append(genOutputPrefix(currentNodeLevel + 1));

        if (confInfo.getCallGraphOutputDetail().equals(Constants.CONFIG_OUTPUT_DETAIL_1)) {
            // # 1: 展示 完整类名+方法名+方法参数
            callerInfo.append(fullMethod);
        } else if (confInfo.getCallGraphOutputDetail().equals(Constants.CONFIG_OUTPUT_DETAIL_2)) {
            // # 2: 展示 完整类名+方法名
            callerInfo.append(callerMethodMap.get(DC.MC_CALLER_FULL_CLASS_NAME))
                    .append(Constants.FLAG_COLON)
                    .append(callerMethodMap.get(DC.MC_CALLER_METHOD_NAME));
        } else {
            // # 3: 展示 简单类名（对于同名类展示完整类名）+方法名
            callerInfo.append(callerMethodMap.get(DC.MC_CALLER_CLASS_NAME))
                    .append(Constants.FLAG_COLON)
                    .append(callerMethodMap.get(DC.MC_CALLER_METHOD_NAME));
        }

        // 添加方法注解信息
        if (confInfo.isShowMethodAnnotation()) {
            String methodAnnotations = methodAnnotationsMap.get(currentCallerMethodHash);
            if (methodAnnotations != null) {
                callerInfo.append(methodAnnotations);
            }
        }

        // 添加循环调用标志
        if (back2Level != Constants.NO_CYCLE_CALL_FLAG) {
            callerInfo.append(String.format(Constants.CALL_FLAG_CYCLE, back2Level));
        }

        Pair<String, Boolean> pair = new MutablePair<>(callerInfo.toString(), Boolean.FALSE);
        callerMethodList.add(pair);
    }

    // 确定写入输出文件的当前被调用方法信息
    private String chooseCallerInfo(String calleeClassName, String calleeFullMethod) {
        if (confInfo.getCallGraphOutputDetail().equals(Constants.CONFIG_OUTPUT_DETAIL_1)) {
            // # 1: 展示 完整类名+方法名+方法参数
            return calleeFullMethod;
        } else if (confInfo.getCallGraphOutputDetail().equals(Constants.CONFIG_OUTPUT_DETAIL_2)) {
            // # 2: 展示 完整类名+方法名
            String calleeFullClassName = CommonUtil.getFullClassNameFromMethod(calleeFullMethod);
            String calleeMethodName = CommonUtil.getOnlyMethodName(calleeFullMethod);
            return calleeFullClassName + Constants.FLAG_COLON + calleeMethodName;
        }
        // # 3: 展示 简单类名（对于同名类展示完整类名）+方法名
        String calleeMethodName = CommonUtil.getOnlyMethodName(calleeFullMethod);
        return calleeClassName + Constants.FLAG_COLON + calleeMethodName;
    }

    // 确定查询被调用关系时所需字段
    private String chooseCallerColumns() {
        if (confInfo.getCallGraphOutputDetail().equals(Constants.CONFIG_OUTPUT_DETAIL_1)) {
            // # 1: 展示 完整类名+方法名+方法参数
            return DC.MC_CALLER_FULL_METHOD;
        } else if (confInfo.getCallGraphOutputDetail().equals(Constants.CONFIG_OUTPUT_DETAIL_2)) {
            // # 2: 展示 完整类名+方法名
            return StringUtils.join(new String[]{
                            DC.MC_CALLER_FULL_METHOD,
                            DC.MC_CALLER_FULL_CLASS_NAME,
                            DC.MC_CALLER_METHOD_NAME
                    }, Constants.FLAG_COMMA_WITH_SPACE
            );
        }
        // # 3: 展示 简单类名（对于同名类展示完整类名）+方法名
        return StringUtils.join(new String[]{
                        DC.MC_CALLER_FULL_METHOD,
                        DC.MC_CALLER_CLASS_NAME,
                        DC.MC_CALLER_METHOD_NAME
                }, Constants.FLAG_COMMA_WITH_SPACE
        );
    }
}
