package com.adrninistrator.jacg.handler.write_db;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbInsertMode;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dboper.DbOperator;
import com.adrninistrator.jacg.dto.write_db.AbstractWriteDbData;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.common.enums.JavaCGOutPutFileTypeEnum;
import com.adrninistrator.javacg.dto.output.JavaCGOutputInfo;
import com.adrninistrator.javacg.exceptions.JavaCGRuntimeException;
import com.adrninistrator.javacg.util.JavaCGFileUtil;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * @author adrninistrator
 * @date 2022/11/15
 * @description: 写入数据库的抽象父类
 */
public abstract class AbstractWriteDbHandler<T extends AbstractWriteDbData> {
    private static final Logger logger = LoggerFactory.getLogger(AbstractWriteDbHandler.class);

    protected final String currentSimpleClassName = this.getClass().getSimpleName();

    protected DbOperWrapper dbOperWrapper;

    // 每次批量写入的数量
    protected int batchSize;

    private final AtomicBoolean failFlag = new AtomicBoolean(false);

    private DbOperator dbOperator;

    // 需要处理的包名/类名前缀
    private Set<String> allowedClassPrefixSet;

    private ThreadPoolExecutor threadPoolExecutor;

    // 任务最大队列数
    private int taskQueueMaxSize;

    // 批量插入数据库记录数
    private int writeRecordNum;

    // 用于统计序号的Map
    private Map<String, Integer> seqMap;

    // 用于生成数据库记录的唯一ID
    private int recordId = 0;

    // 需要读取的文件是属于主要的文件还是其他的文件
    private final boolean mainFile;

    // 需要读取的主要文件类型
    private final JavaCGOutPutFileTypeEnum mainFileTypeEnum;

    // 需要读取的其他文件名称
    private final String otherFileName;

    // 需要读取的文件最小列数
    private final int minColumnNum;

    // 需要读取的文件最大列数
    private final int maxColumnNum;

    // 需要写到的数据库表信息
    private final DbTableInfoEnum dbTableInfoEnum;

    // 当前需要读取的文件名称
    private String fileName;

    // 当前需要读取的文件描述
    private String fileDesc;

    public AbstractWriteDbHandler() {
        JACGWriteDbHandler jacgWriteDbHandler = this.getClass().getAnnotation(JACGWriteDbHandler.class);
        if (jacgWriteDbHandler == null) {
            logger.error("类缺少注解 {}", this.getClass().getName());
            throw new JavaCGRuntimeException("类缺少注解");
        }

        // 是否需要读取文件
        boolean readFile = jacgWriteDbHandler.readFile();
        mainFile = jacgWriteDbHandler.mainFile();
        mainFileTypeEnum = jacgWriteDbHandler.mainFileTypeEnum();
        otherFileName = jacgWriteDbHandler.otherFileName();
        minColumnNum = jacgWriteDbHandler.minColumnNum();
        maxColumnNum = jacgWriteDbHandler.maxColumnNum();
        dbTableInfoEnum = jacgWriteDbHandler.dbTableInfoEnum();

        if (dbTableInfoEnum == null) {
            logger.error("类的注解未配置对应的数据库表信息 {}", this.getClass().getName());
            throw new JavaCGRuntimeException();
        }

        if (!readFile) {
            return;
        }

        if ((minColumnNum == 0 || maxColumnNum == 0
                || (mainFile && (mainFileTypeEnum == null || JavaCGOutPutFileTypeEnum.OPFTE_ILLEGAL == mainFileTypeEnum))
                || (!mainFile && StringUtils.isBlank(otherFileName))
        )) {
            logger.error("类需要读取文件但配置错误 {}", this.getClass().getName());
            throw new JavaCGRuntimeException();
        }

        if (DbTableInfoEnum.DTIE_ILLEGAL != dbTableInfoEnum) {
            String[] fileColumnDesc = chooseFileColumnDesc();
            if (ArrayUtils.isEmpty(fileColumnDesc)) {
                logger.error("当前处理的文件列的描述为空 {}", this.getClass().getName());
                throw new JavaCGRuntimeException();
            }

            if (fileColumnDesc.length != maxColumnNum) {
                logger.error("当前处理的文件列的描述数量与最大列数不同 {} {} {}", this.getClass().getName(), fileColumnDesc.length, maxColumnNum);
                throw new JavaCGRuntimeException();
            }
        }

        fileName = mainFile ? mainFileTypeEnum.getFileName() : otherFileName;
        fileDesc = mainFile ? mainFileTypeEnum.getDesc() : chooseOtherFileDesc();
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
     * 对生成数据的自定义处理
     *
     * @param data
     */
    protected void handleData(T data) {
    }

    /**
     * 返回当前处理的文件列的描述
     * 假如子类需要读取文件，则需要重载当前方法
     *
     * @return
     */
    public String[] chooseFileColumnDesc() {
        throw new JavaCGRuntimeException("不会调用当前方法");
    }

    /**
     * 返回需要读取的其他文件描述
     *
     * @return
     */
    public String chooseOtherFileDesc() {
        return null;
    }

    /**
     * 返回需要读取的其他文件的详细说明
     *
     * @return
     */
    public String[] chooseOtherFileDetailInfo() {
        return null;
    }

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
        String[] array = StringUtils.splitPreserveAllTokens(line, JavaCGConstants.FILE_COLUMN_SEPARATOR);
        if (array.length != rowNum) {
            logger.error("{} 文件列数非法 预期: {} 实际: {} [{}]", currentSimpleClassName, rowNum, array.length, line);
            throw new JavaCGRuntimeException(currentSimpleClassName + "文件列数非法");
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
        String[] array = StringUtils.splitPreserveAllTokens(line, JavaCGConstants.FILE_COLUMN_SEPARATOR, maxRowNum);
        if (array.length < minRowNum) {
            logger.error("{} 文件列数非法 {} [{}]", currentSimpleClassName, array.length, line);
            throw new JavaCGRuntimeException(currentSimpleClassName + "文件列数非法");
        }

        return array;
    }

    /**
     * 执行完毕之前的操作
     */
    protected void beforeDone() {
    }

    /**
     * 读取文件并写入数据库
     *
     * @param javaCGOutputInfo
     * @return
     */
    public boolean handle(JavaCGOutputInfo javaCGOutputInfo) {
        List<T> dataList = new ArrayList<>(batchSize);

        String filePath = mainFile ? javaCGOutputInfo.getMainFilePath(mainFileTypeEnum) : javaCGOutputInfo.getOtherFilePath(otherFileName);
        try (BufferedReader br = JavaCGFileUtil.genBufferedReader(filePath)) {
            String line;
            while ((line = br.readLine()) != null) {
                if (StringUtils.isBlank(line)) {
                    continue;
                }

                String[] lineArray;
                if (minColumnNum == maxColumnNum) {
                    lineArray = splitEquals(line, minColumnNum);
                } else {
                    lineArray = splitBetween(line, minColumnNum, maxColumnNum);
                }

                // 根据读取的文件内容生成对应对象
                T data = genData(lineArray);
                if (data == null) {
                    continue;
                }

                // 对生成数据的自定义处理
                handleData(data);

                dataList.add(data);
                // 将数据写入数据库
                tryInsertDb(dataList);
            }

            // 结束前将剩余数据写入数据库
            insertDb(dataList);

            // 执行完毕之前的操作
            beforeDone();
            return true;
        } catch (Exception e) {
            logger.error("error ", e);
            return false;
        }
    }

    /**
     * 尝试将数据写入数据库
     *
     * @param dataList
     */
    public void tryInsertDb(List<T> dataList) {
        if (dataList.size() >= batchSize) {
            insertDb(dataList);
        }
    }

    /**
     * 将数据写入数据库
     *
     * @param dataList
     */
    public void insertDb(List<T> dataList) {
        if (dataList.isEmpty()) {
            return;
        }

        writeRecordNum += dataList.size();
        if (logger.isDebugEnabled()) {
            logger.debug("写入数据库 {} {}", currentSimpleClassName, dataList.size());
        }
        // 生成用于插入数据的sql语句
        String sql = dbOperWrapper.genAndCacheInsertSql(dbTableInfoEnum, DbInsertMode.DIME_INSERT);

        // 根据需要写入的数据生成Object数组
        List<Object[]> objectList = new ArrayList<>(dataList.size());
        for (T data : dataList) {
            // genObjectArray()方法在当前线程中执行，没有线程安全问题
            objectList.add(genObjectArray(data));
        }

        // 等待直到允许任务执行
        JACGUtil.wait4TPEExecute(threadPoolExecutor, taskQueueMaxSize);

        threadPoolExecutor.execute(() -> {
            // 指量写入数据库
            if (!dbOperator.batchInsert(sql, objectList)) {
                failFlag.set(true);
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
        return failFlag.get();
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
     * 获取下一个记录ID
     *
     * @return
     */
    protected int genNextRecordId() {
        return ++recordId;
    }

    //
    public String getCurrentSimpleClassName() {
        return currentSimpleClassName;
    }

    public int getWriteRecordNum() {
        return writeRecordNum;
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

    public void setBatchSize(int batchSize) {
        this.batchSize = batchSize;
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
