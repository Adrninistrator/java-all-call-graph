package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbInsertMode;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.exceptions.JACGSQLException;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dboper.DbOperator;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.common.enums.JavaCGOutPutFileTypeEnum;
import com.adrninistrator.javacg.dto.counter.JavaCGCounter;
import com.adrninistrator.javacg.dto.output.JavaCGOutputInfo;
import com.adrninistrator.javacg.exceptions.JavaCGRuntimeException;
import com.adrninistrator.javacg.util.JavaCGClassMethodUtil;
import com.adrninistrator.javacg.util.JavaCGFileUtil;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ThreadPoolExecutor;

/**
 * @author adrninistrator
 * @date 2022/11/15
 * @description: 写入数据库的抽象父类
 */
public abstract class AbstractWriteDbHandler<T extends BaseWriteDbData> {
    private static final Logger logger = LoggerFactory.getLogger(AbstractWriteDbHandler.class);

    protected final String currentSimpleClassName = this.getClass().getSimpleName();

    private final WriteDbResult writeDbResult;

    // 是否需要读取java-callgraph2生成的文件
    private final boolean readFile;

    // 需要读取的文件是属于主要的文件还是其他的文件
    private final boolean mainFile;

    // 需要读取的主要文件类型
    private final JavaCGOutPutFileTypeEnum mainFileTypeEnum;

    // 需要读取的其他文件名称
    private final String otherFileName;

    // 需要写入到文件的名称
    private final String writeFileName;

    // 需要读取的文件最小列数
    private final int minColumnNum;

    // 需要读取的文件最大列数
    private final int maxColumnNum;

    // 读取文件时是否允许最后一列出现TAB
    private final boolean allowTabInLastColumn;

    // 需要写到的数据库表信息
    private final DbTableInfoEnum dbTableInfoEnum;

    // 初始化标记
    private boolean inited;

    // 当前需要读取的文件名称
    private String fileName;

    // 当前需要读取的文件描述
    private String fileDesc;

    // 记录处理失败的次数
    private JavaCGCounter failNum;

    // 记录写数据库记录的次数
    private JavaCGCounter writeDbNum;

    // 记录写文件记录的次数
    private JavaCGCounter writeFileNum;

    protected DbOperWrapper dbOperWrapper;

    // 每次批量写入的数量
    protected int dbInsertBatchSize;

    private DbOperator dbOperator;

    // 需要处理的包名/类名前缀
    private Set<String> allowedClassPrefixSet;

    private ThreadPoolExecutor threadPoolExecutor;

    // 任务最大队列数
    private int taskQueueMaxSize;

    // 用于生成数据库记录的唯一ID
    private int recordId = 0;

    // 保存需要写入数据库的数据列表
    private List<T> dataList;

    // 用于统计序号的Map
    private Map<String, Integer> seqMap;

    // 若当前实例不是从文件读取，则将获取的信息写入对应文件
    protected Writer fileWriter;

    public AbstractWriteDbHandler(WriteDbResult writeDbResult) {
        this.writeDbResult = writeDbResult;

        JACGWriteDbHandler jacgWriteDbHandler = this.getClass().getAnnotation(JACGWriteDbHandler.class);
        if (jacgWriteDbHandler == null) {
            logger.error("类缺少注解 {} {}", currentSimpleClassName, JACGWriteDbHandler.class.getName());
            throw new JavaCGRuntimeException("类缺少注解");
        }

        // 是否需要读取文件
        readFile = jacgWriteDbHandler.readFile();
        mainFile = jacgWriteDbHandler.mainFile();
        mainFileTypeEnum = jacgWriteDbHandler.mainFileTypeEnum();
        otherFileName = jacgWriteDbHandler.otherFileName();
        writeFileName = jacgWriteDbHandler.writeFileEnum().getName();
        minColumnNum = jacgWriteDbHandler.minColumnNum();
        maxColumnNum = jacgWriteDbHandler.maxColumnNum();
        allowTabInLastColumn = jacgWriteDbHandler.allowTabInLastColumn();
        dbTableInfoEnum = jacgWriteDbHandler.dbTableInfoEnum();

        if (dbTableInfoEnum == null) {
            logger.error("类的注解未配置对应的数据库表信息 {}", currentSimpleClassName);
            throw new JavaCGRuntimeException("类的注解未配置对应的数据库表信息");
        }

        if (readFile) {
            if ((minColumnNum == 0 || maxColumnNum == 0
                    || (mainFile && (mainFileTypeEnum == null || JavaCGOutPutFileTypeEnum.OPFTE_ILLEGAL == mainFileTypeEnum))
                    || (!mainFile && StringUtils.isBlank(otherFileName))
            )) {
                logger.error("类需要读取文件但配置错误 {}", currentSimpleClassName);
                throw new JavaCGRuntimeException("类需要读取文件但配置错误");
            }
        } else {
            if (StringUtils.isBlank(writeFileName) || mainFile || (mainFileTypeEnum != null && JavaCGOutPutFileTypeEnum.OPFTE_ILLEGAL != mainFileTypeEnum)
                    || StringUtils.isNotBlank(otherFileName)) {
                logger.error("类不需要读取文件但配置错误 {}", currentSimpleClassName);
                throw new JavaCGRuntimeException("类不需要读取文件但配置错误");
            }
        }
        if (DbTableInfoEnum.DTIE_ILLEGAL == dbTableInfoEnum) {
            return;
        }

        String[] fileColumnDesc = chooseFileColumnDesc();
        if (ArrayUtils.isEmpty(fileColumnDesc)) {
            logger.error("当前处理的文件列的描述为空 {}", currentSimpleClassName);
            throw new JavaCGRuntimeException("当前处理的文件列的描述为空");
        }
        if (mainFile) {
            if (fileColumnDesc.length != maxColumnNum) {
                logger.error("当前处理的文件列的描述数量与最大列数不同 {} {} {}", currentSimpleClassName, fileColumnDesc.length, maxColumnNum);
                throw new JavaCGRuntimeException("当前处理的文件列的描述数量与最大列数不同");
            }
            fileName = mainFileTypeEnum.getFileName();
            fileDesc = mainFileTypeEnum.getDesc();
        } else {
            fileName = readFile ? otherFileName : writeFileName;
            fileDesc = chooseNotMainFileDesc();
        }
        if (StringUtils.isBlank(fileDesc)) {
            logger.error("当前类未定义文件描述 {}", currentSimpleClassName);
            throw new JavaCGRuntimeException("当前类未定义文件描述");
        }
        String[] fileDetailInfoArray = chooseFileDetailInfo();
        if (ArrayUtils.isEmpty(fileDetailInfoArray)) {
            logger.error("当前类未定义文件详情 {}", currentSimpleClassName);
            throw new JavaCGRuntimeException("当前类未定义文件详情");
        }

        // 由于不同的类可能写相同的数据库表，因此以下使用computeIfAbsent
        writeDbNum = writeDbResult.getWriteDbNumMap().computeIfAbsent(dbTableInfoEnum.getTableNameKeyword(), k -> new JavaCGCounter(0));
        if (!readFile) {
            if (fileColumnDesc.length != dbTableInfoEnum.getColumns().length) {
                logger.error("当前处理的文件列的描述数量与数据库表字段数量不同 {} {} {}", currentSimpleClassName, fileColumnDesc.length, dbTableInfoEnum.getColumns().length);
                throw new JavaCGRuntimeException("当前处理的文件列的描述数量与数据库表字段数量不同");
            }

            writeFileNum = new JavaCGCounter(0);
            JavaCGCounter existedWriteFileNum = writeDbResult.getWriteFileNumMap().putIfAbsent(writeFileName, writeFileNum);
            if (existedWriteFileNum != null) {
                logger.error("当前写文件的信息已被设置过 {} {}", currentSimpleClassName, writeFileName);
                throw new JavaCGRuntimeException("当前写文件的信息已被设置过");
            }
        }
        failNum = new JavaCGCounter(0);
        JavaCGCounter existedFailNum = writeDbResult.getFailNumMap().putIfAbsent(currentSimpleClassName, failNum);
        if (existedFailNum != null) {
            // 这里检查了当前类名是否存在重复，后续不需要再检查
            logger.error("当前类的失败记数器被设置过 {}", currentSimpleClassName);
            throw new JavaCGRuntimeException("当前类的失败记数器被设置过");
        }
    }

    /**
     * 自定义初始化操作
     */
    public void init() {
        dataList = new ArrayList<>(dbInsertBatchSize);
        inited = true;
    }

    /**
     * 根据读取的文件内容生成对应对象
     * 假如子类需要读取文件，则需要重载当前方法
     *
     * @param lineArray 文件行内容，已处理为数组形式
     * @return 返回null代表当前行不需要处理；返回非null代表需要处理
     */
    protected T genData(String[] lineArray) {
        throw new JavaCGRuntimeException("不会调用当前方法");
    }

    /**
     * 返回当前处理的文件列的描述
     * 假如子类需要读取文件，则需要重载当前方法
     *
     * @return
     */
    public abstract String[] chooseFileColumnDesc();

    /**
     * 返回非主要文件的描述，包括需要读取的其他文件、需要写文件的情况
     *
     * @return
     */
    public String chooseNotMainFileDesc() {
        return null;
    }

    /**
     * 返回需要读取的文件的详细说明
     *
     * @return
     */
    public abstract String[] chooseFileDetailInfo();

    /**
     * 根据需要写入的数据生成Object数组
     * 在当前线程中执行，没有线程安全问题
     *
     * @param data
     * @return
     */
    protected abstract Object[] genObjectArray(T data);

    /**
     * 初始化用于统计序号的Map
     */
    protected void initSeqMap() {
        seqMap = new HashMap<>();
    }

    /**
     * 对文件行内容进行分割，结果数需要等于指定值
     *
     * @param line
     * @param rowNum
     * @return
     */
    protected String[] splitEquals(String line, int rowNum) {
        String[] array = StringUtils.splitPreserveAllTokens(line, JavaCGConstants.FLAG_TAB);
        if (array.length != rowNum) {
            logger.error("{} 文件列数非法 预期: {} 实际: {} [{}]", currentSimpleClassName, rowNum, array.length, line);
            throw new JavaCGRuntimeException(currentSimpleClassName + " 文件列数非法");
        }

        return array;
    }

    /**
     * 对文件行内容进行分割，指定最大及最小的列数
     *
     * @param line
     * @param minRowNum 最小列数
     * @param maxRowNum 最大列数
     * @return
     */
    protected String[] splitBetween(String line, int minRowNum, int maxRowNum) {
        String[] array = StringUtils.splitPreserveAllTokens(line, JavaCGConstants.FLAG_TAB, maxRowNum);
        if (array.length < minRowNum) {
            logger.error("{} 文件列数非法 {} [{}]", currentSimpleClassName, array.length, line);
            throw new JavaCGRuntimeException(currentSimpleClassName + "文件列数非法");
        }

        return array;
    }

    /**
     * 执行开始之前的操作
     */
    public void beforeHandle(String javaCgOutputPath) throws FileNotFoundException {
        if (!readFile) {
            String outputFilePath = javaCgOutputPath + writeFileName;
            logger.info("{} 将结果写入文件 {}", currentSimpleClassName, outputFilePath);
            fileWriter = JavaCGFileUtil.genBufferedWriter(outputFilePath);
        }
    }

    /**
     * 执行完成之后的操作
     */
    public void afterHandle() {
        // 结束前将剩余数据写入数据库
        insertDb();

        if (!readFile && fileWriter != null) {
            IOUtils.closeQuietly(fileWriter);
        }
        writeDbResult.getAfterHandleMap().put(currentSimpleClassName, Boolean.TRUE);
    }

    private void checkInited() {
        if (!inited) {
            logger.error("需要先调用 init 方法后再调用当前方法");
            throw new JavaCGRuntimeException("需要先调用 init 方法后再调用当前方法");
        }
    }

    /**
     * 读取文件并写入数据库
     *
     * @param javaCGOutputInfo
     * @return
     */
    public boolean handle(JavaCGOutputInfo javaCGOutputInfo) {
        checkInited();

        String filePath = mainFile ? javaCGOutputInfo.getMainFilePath(mainFileTypeEnum) : javaCGOutputInfo.getOtherFilePath(otherFileName);
        try (BufferedReader br = JavaCGFileUtil.genBufferedReader(filePath)) {
            // 执行开始之前的操作
            beforeHandle(javaCGOutputInfo.getOutputDirPath());

            String line;
            while ((line = br.readLine()) != null) {
                if (StringUtils.isBlank(line)) {
                    continue;
                }

                String[] lineArray;
                if (minColumnNum == maxColumnNum && !allowTabInLastColumn) {
                    lineArray = splitEquals(line, minColumnNum);
                } else {
                    lineArray = splitBetween(line, minColumnNum, maxColumnNum);
                }

                // 根据读取的文件内容生成对应对象
                T data = genData(lineArray);
                if (data == null) {
                    continue;
                }

                dataList.add(data);
                // 将数据写入数据库
                tryInsertDb();
            }

            return !checkFailed();
        } catch (Exception e) {
            logger.error("error ", e);
            return false;
        } finally {
            // 执行完成之后的操作
            afterHandle();
        }
    }

    /**
     * 尝试将数据写入数据库
     */
    public void tryInsertDb() {
        checkInited();

        if (dataList.size() >= dbInsertBatchSize) {
            insertDb();
        }
    }

    /**
     * 将数据写入数据库
     */
    protected void insertDb() {
        checkInited();

        if (dataList.isEmpty()) {
            return;
        }

        if (writeDbNum == null) {
            logger.error("记录写入数据库记录数量的对象未生成，需要先调用 beforeHandle 方法创建 {}", currentSimpleClassName);
            throw new JavaCGRuntimeException("记录写入数据库记录数量的对象未生成，需要先调用 beforeHandle 方法创建");
        }
        writeDbNum.addAndGet(dataList.size());

        if (!readFile) {
            if (writeFileNum == null) {
                logger.error("记录写入文件记录数量的对象未生成，需要先调用 beforeHandle 方法创建 {}", currentSimpleClassName);
                throw new JavaCGRuntimeException("记录写入文件记录数量的对象未生成，需要先调用 beforeHandle 方法创建");
            }
            writeFileNum.addAndGet(dataList.size());
        }
        if (logger.isDebugEnabled()) {
            logger.debug("写入数据库 {} {}", currentSimpleClassName, dataList.size());
        }
        // 生成用于插入数据的sql语句
        String sql = dbOperWrapper.genAndCacheInsertSql(dbTableInfoEnum, DbInsertMode.DIME_INSERT);

        // 根据需要写入的数据生成Object数组
        List<Object[]> objectList = new ArrayList<>(dataList.size());
        for (T data : dataList) {
            // genObjectArray()方法在当前线程中执行，没有线程安全问题
            Object[] objects = genObjectArray(data);
            if (!readFile) {
                if (fileWriter == null) {
                    logger.error("写入文件的对象未生成，需要先调用 beforeHandle 方法创建 {}", currentSimpleClassName);
                    failNum.addAndGet();
                    throw new JavaCGRuntimeException("写入文件的对象未生成，需要先调用 beforeHandle 方法创建");
                }

                try {
                    fileWriter.write(StringUtils.join(objects, JavaCGConstants.FLAG_TAB) + JavaCGConstants.NEW_LINE);
                } catch (Exception e) {
                    logger.error("写文件出现异常 {} ", currentSimpleClassName, e);
                }
            }
            objectList.add(objects);
        }

        // 等待直到允许任务执行
        JACGUtil.wait4TPEExecute(threadPoolExecutor, taskQueueMaxSize);

        threadPoolExecutor.execute(() -> {
            // 指量写入数据库
            try {
                if (!dbOperator.batchInsert(sql, objectList)) {
                    failNum.addAndGet();
                }
            } catch (JACGSQLException e) {
                failNum.addAndGet();
            }
        });
        dataList.clear();
    }

    /**
     * 判断是否为需要处理的包名/类名前缀
     *
     * @param className 类名，或完整方法（类名+方法名+参数）
     * @return true: 需要处理，false: 忽略
     */
    protected boolean isAllowedClassPrefix(String className) {
        if (allowedClassPrefixSet.isEmpty()) {
            return true;
        }

        for (String allowedClassPrefix : allowedClassPrefixSet) {
            if (className.startsWith(allowedClassPrefix)) {
                return true;
            }
        }
        return false;
    }

    /**
     * 返回读文件写数据库是否失败
     *
     * @return false: 未失败 true: 失败
     */
    public boolean checkFailed() {
        if (DbTableInfoEnum.DTIE_ILLEGAL == dbTableInfoEnum) {
            return false;
        }
        return failNum.getCount() > 0;
    }

    /**
     * 生成下一个序号，从0开始
     *
     * @param key
     * @return
     */
    protected Integer genNextSeq(String key) {
        Integer seq = seqMap.computeIfAbsent(key, k -> -1);
        seq++;
        seqMap.put(key, seq);
        return seq;
    }

    /**
     * 获取下一个记录ID，从1开始
     *
     * @return
     */
    protected int genNextRecordId() {
        return ++recordId;
    }

    public void addData(T data) {
        dataList.add(data);
    }

    /**
     * 判断指定方法是否属于dto的get/set方法，若当前类未找到对应的方法，则在超类中查找
     *
     * @param getMethod                  true: 当前为get方法 false: 当前为set方法
     * @param simpleClassName            唯一类名
     * @param getSetMethodName           get/set方法名
     * @param getSetMethodSimpleClassMap dto的get/set方法Map
     * @param extendsSimpleClassNameMap  涉及继承的唯一类名
     * @return true: 是dto的get/set方法 false: 不是dto的get/set方法
     */
    protected boolean checkDtoGetSetMethod(boolean getMethod, String simpleClassName, String getSetMethodName, Map<String, Set<String>> getSetMethodSimpleClassMap, Map<String,
            String> extendsSimpleClassNameMap) {
        if ((getMethod && !JavaCGClassMethodUtil.matchesGetMethod(getSetMethodName)) || (!getMethod && !JavaCGClassMethodUtil.matchesSetMethod(getSetMethodName))) {
            // 根据方法名称判断不是get/set方法
            return false;
        }

        String currentSimpleClassName = simpleClassName;
        while (true) {
            // 当前当前类的get/set方法是否是dto的get/set方法
            Set<String> methodSet = getSetMethodSimpleClassMap.get(currentSimpleClassName);
            if (methodSet != null && methodSet.contains(getSetMethodName)) {
                return true;
            }
            // 获取当前类的父类
            String superSimpleClassName = extendsSimpleClassNameMap.get(currentSimpleClassName);
            if (superSimpleClassName == null) {
                return false;
            }
            currentSimpleClassName = superSimpleClassName;
        }
    }

    //
    public int getRecordId() {
        return recordId;
    }

    public void setRecordId(int recordId) {
        this.recordId = recordId;
    }

    public String getCurrentSimpleClassName() {
        return currentSimpleClassName;
    }

    public String getFileName() {
        return fileName;
    }

    public String getFileDesc() {
        return fileDesc;
    }

    //
    public void setDbOperWrapper(DbOperWrapper dbOperWrapper) {
        this.dbOperWrapper = dbOperWrapper;
    }

    public void setDbOperator(DbOperator dbOperator) {
        this.dbOperator = dbOperator;
    }

    public void setDbInsertBatchSize(int dbInsertBatchSize) {
        this.dbInsertBatchSize = dbInsertBatchSize;
    }

    public void setAllowedClassPrefixSet(Set<String> allowedClassPrefixSet) {
        this.allowedClassPrefixSet = allowedClassPrefixSet;
    }

    public void setThreadPoolExecutor(ThreadPoolExecutor threadPoolExecutor) {
        this.threadPoolExecutor = threadPoolExecutor;
    }

    public void setTaskQueueMaxSize(int taskQueueMaxSize) {
        this.taskQueueMaxSize = taskQueueMaxSize;
    }
}
