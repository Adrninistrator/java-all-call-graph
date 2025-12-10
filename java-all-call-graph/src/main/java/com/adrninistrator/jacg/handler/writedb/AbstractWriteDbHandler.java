package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbInsertMode;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.WriteDbHandlerWriteFileEnum;
import com.adrninistrator.jacg.common.exceptions.JACGSQLException;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dboper.DbOperator;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData;
import com.adrninistrator.jacg.neo4j.util.JACGNeo4jUtil;
import com.adrninistrator.jacg.spring.context.SpringContextManager;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.common.enums.JavaCG2OutPutFileTypeEnum;
import com.adrninistrator.javacg2.dto.counter.JavaCG2Counter;
import com.adrninistrator.javacg2.dto.output.JavaCG2OutputInfo;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;
import com.adrninistrator.javacg2.util.JavaCG2FileUtil;
import com.adrninistrator.javacg2.util.JavaCG2ThreadUtil;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.data.neo4j.repository.Neo4jRepository;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * @author adrninistrator
 * @date 2022/11/15
 * @description: 写入数据库的抽象父类
 */
public abstract class AbstractWriteDbHandler<T extends BaseWriteDbData> {
    private static final Logger logger = LoggerFactory.getLogger(AbstractWriteDbHandler.class);

    protected final String currentSimpleClassName = this.getClass().getSimpleName();

    // 当前类上的 JACGWriteDbHandler 注解
    private JACGWriteDbHandler jacgWriteDbHandler;

    // 需要读取的文件是属于主要的文件还是其他的文件
    private final boolean mainFile;

    // 是否需要写文件
    private final boolean writeFile;

    // 需要读取的主要文件类型
    private final JavaCG2OutPutFileTypeEnum mainFileTypeEnum;

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

    // 是否完成初始化检查标记
    private boolean initChecked;

    // 当前需要读取的文件名称
    private String fileName;

    // 当前需要读取的文件描述
    private String fileDesc;

    // 记录处理失败的次数
    private JavaCG2Counter failNum;

    // 记录写数据库记录的次数
    private JavaCG2Counter writeDbNum;

    // 记录写文件记录的次数
    private JavaCG2Counter writeFileNum;

    // 记录写数据库结果
    private WriteDbResult writeDbResult;

    protected DbOperWrapper dbOperWrapper;

    // 是否使用PostgreSQL数据库
    protected boolean usePg;

    // 每次批量写入的数量
    protected int dbInsertBatchSize;

    private DbOperator dbOperator;

    protected String appName;

    private ThreadPoolExecutor threadPoolExecutor;

    private AtomicInteger runningTaskNum;

    // 任务最大队列数
    private int taskQueueMaxSize;

    // 用于生成数据库记录的唯一ID
    private int recordId = 0;

    // 保存需要写入数据库的数据列表
    protected List<T> dataList;

    // 用于统计序号的Map
    private Map<String, Integer> seqMap;

    // 当前读取到的文件行数组
    private String[] lineArray;

    // 当前处理的文件行数组序号
    private int lineArrayIndex = 0;

    // 是否跳过依赖的处理类写入顺序
    private boolean skipDependsCheck = false;

    // 若当前实例不是从文件读取，则将获取的信息写入对应文件
    protected Writer fileWriter;

    protected Neo4jRepository neo4jRepository;

    protected ApplicationContext applicationContext;

    public AbstractWriteDbHandler(WriteDbResult writeDbResult) {
        jacgWriteDbHandler = this.getClass().getAnnotation(JACGWriteDbHandler.class);
        if (jacgWriteDbHandler == null) {
            // 假如类上未获取到注解，则使用父类上的
            jacgWriteDbHandler = this.getClass().getSuperclass().getAnnotation(JACGWriteDbHandler.class);
        }
        if (jacgWriteDbHandler == null) {
            logger.error("类及父类缺少注解 {} {}", currentSimpleClassName, JACGWriteDbHandler.class.getName());
            throw new JavaCG2RuntimeException("类及父类缺少注解");
        }

        // 是否需要读取java-callgraph2生成的文件
        boolean readFile = jacgWriteDbHandler.readFile();
        mainFile = jacgWriteDbHandler.mainFile();
        mainFileTypeEnum = jacgWriteDbHandler.mainFileTypeEnum();
        otherFileName = jacgWriteDbHandler.otherFileName();
        // 是否读取其他文件
        boolean readOtherFile = StringUtils.isNotBlank(otherFileName);
        writeFile = (WriteDbHandlerWriteFileEnum.WDHWFE_NONE != jacgWriteDbHandler.writeFileEnum());
        writeFileName = jacgWriteDbHandler.writeFileEnum().getName();
        minColumnNum = jacgWriteDbHandler.minColumnNum();
        maxColumnNum = jacgWriteDbHandler.maxColumnNum();
        allowTabInLastColumn = jacgWriteDbHandler.allowTabInLastColumn();
        dbTableInfoEnum = jacgWriteDbHandler.dbTableInfoEnum();

        if (dbTableInfoEnum == null) {
            logger.error("类的注解未配置对应的数据库表信息 {}", currentSimpleClassName);
            throw new JavaCG2RuntimeException("类的注解未配置对应的数据库表信息");
        }

        if (readFile) {
            if ((minColumnNum == 0 || maxColumnNum == 0
                    || (mainFile && (mainFileTypeEnum == null || JavaCG2OutPutFileTypeEnum.OPFTE_ILLEGAL == mainFileTypeEnum))
                    || (!mainFile && !readOtherFile)
            )) {
                logger.error("类需要读取文件但配置错误 {}", currentSimpleClassName);
                throw new JavaCG2RuntimeException("类需要读取文件但配置错误");
            }
        } else {
            if (mainFile || (mainFileTypeEnum != null && JavaCG2OutPutFileTypeEnum.OPFTE_ILLEGAL != mainFileTypeEnum) || readOtherFile) {
                logger.error("类不需要读取文件但配置错误 {}", currentSimpleClassName);
                throw new JavaCG2RuntimeException("类不需要读取文件但配置错误");
            }
        }

        if (ArrayUtils.isEmpty(dbTableInfoEnum.getColumns())) {
            return;
        }

        String[] fileColumnDesc = chooseFileColumnDesc();
        if (readFile) {
            // 需要读取文件
            if (fileColumnDesc.length != maxColumnNum) {
                logger.error("当前处理的文件列的描述数量与最大列数不同 {} {} {}", currentSimpleClassName, fileColumnDesc.length, maxColumnNum);
                throw new JavaCG2RuntimeException("当前处理的文件列的描述数量与最大列数不同");
            }
        }

        if (mainFile) {
            fileName = mainFileTypeEnum.getFileName();
            fileDesc = mainFileTypeEnum.getDesc();
        } else {
            fileName = readFile ? otherFileName : writeFileName;
            fileDesc = chooseNotMainFileDesc();
        }

        if (readFile || writeFile) {
            // 需要读取或者写文件
            String[] fileDetailInfoArray = chooseFileDetailInfo();
            if (ArrayUtils.isEmpty(fileDetailInfoArray)) {
                logger.error("当前类未定义文件详情 {}", currentSimpleClassName);
                throw new JavaCG2RuntimeException("当前类未定义文件详情");
            }

            if (ArrayUtils.isEmpty(fileColumnDesc)) {
                logger.error("当前处理的文件列的描述为空 {}", currentSimpleClassName);
                throw new JavaCG2RuntimeException("当前处理的文件列的描述为空");
            }

            if (StringUtils.isBlank(fileDesc)) {
                logger.error("当前类未定义文件描述 {}", currentSimpleClassName);
                throw new JavaCG2RuntimeException("当前类未定义文件描述");
            }
        }

        // 由于不同的类可能写相同的数据库表，因此以下使用computeIfAbsent
        writeDbNum = writeDbResult.getWriteDbNumMap().computeIfAbsent(dbTableInfoEnum.getTableNameKeyword(), k -> new JavaCG2Counter(0));
        if (writeFile) {
            if (fileColumnDesc.length != dbTableInfoEnum.getColumns().length) {
                logger.error("当前处理的文件列的描述数量与数据库表字段数量不同 {} {} {}", currentSimpleClassName, fileColumnDesc.length, dbTableInfoEnum.getColumns().length);
                throw new JavaCG2RuntimeException("当前处理的文件列的描述数量与数据库表字段数量不同");
            }
            writeFileNum = new JavaCG2Counter(0);
            JavaCG2Counter existedWriteFileNum = writeDbResult.getWriteFileNumMap().putIfAbsent(writeFileName, writeFileNum);
            if (existedWriteFileNum != null) {
                logger.error("当前写文件的信息已被设置过 {} {}", currentSimpleClassName, writeFileName);
                throw new JavaCG2RuntimeException("当前写文件的信息已被设置过");
            }
        }

        failNum = new JavaCG2Counter(0);
        JavaCG2Counter existedFailNum = writeDbResult.getFailNumMap().putIfAbsent(currentSimpleClassName, failNum);
        if (existedFailNum != null) {
            // 这里检查了当前类名是否存在重复，后续不需要再检查
            logger.error("当前类的失败记数器被设置过 {}", currentSimpleClassName);
            throw new JavaCG2RuntimeException("当前类的失败记数器被设置过");
        }
    }

    // 是否使用neo4j
    protected boolean useNeo4j() {
        return false;
    }

    // 使用neo4j时，选择当前类使用的Neo4jRepository
    protected Class<Neo4jRepository> chooseNeo4jRepository() {
        throw new JavaCG2RuntimeException("子类需要重载当前方法 " + currentSimpleClassName);
    }

    // 使用neo4j时，将当前解析的数据转换为Neo4jDomain
    protected Object transferNeo4jDomain(T data) {
        throw new JavaCG2RuntimeException("子类需要重载当前方法 " + currentSimpleClassName);
    }

    /**
     * 自定义初始化操作
     */
    public void init(WriteDbResult writeDbResult) {
        this.writeDbResult = writeDbResult;
        dataList = new ArrayList<>(dbInsertBatchSize);
        inited = true;

        if (useNeo4j()) {
            applicationContext = SpringContextManager.getApplicationContext();
            Class<Neo4jRepository> neo4jRepositoryClass = chooseNeo4jRepository();
            neo4jRepository = applicationContext.getBean(neo4jRepositoryClass);
        }

        writeDbResult.getWriteDbHandlerMap().put(currentSimpleClassName, this);

        if (dbOperator != null) {
            usePg = dbOperator.getDbConfInfo().isUsePgDb();
        }
    }

    /**
     * 根据读取的文件内容生成对应对象
     * 假如子类需要读取文件，则需要重载当前方法
     *
     * @param lineArray 文件行内容，已处理为数组形式
     * @return 返回null代表当前行不需要处理；返回非null代表需要处理
     */
    protected T genData(String[] lineArray) {
        throw new JavaCG2RuntimeException("不会调用当前方法");
    }

    /**
     * 读取当前行文件数组内容
     *
     * @return
     */
    protected String readLineData() {
        String data = lineArray[lineArrayIndex];
        lineArrayIndex++;
        return data;
    }

    /**
     * 返回当前处理的文件列的描述
     * 假如子类需要读取文件，则需要重载当前方法
     *
     * @return
     */
    public String[] chooseFileColumnDesc() {
        return null;
    }

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
    public String[] chooseFileDetailInfo() {
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
        String[] array = StringUtils.splitPreserveAllTokens(line, JavaCG2Constants.FLAG_TAB);
        if (array.length != rowNum) {
            logger.error("{} 文件列数非法 预期: {} 实际: {} [{}]", currentSimpleClassName, rowNum, array.length, line);
            throw new JavaCG2RuntimeException(currentSimpleClassName + " 文件列数非法");
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
        String[] array = StringUtils.splitPreserveAllTokens(line, JavaCG2Constants.FLAG_TAB, maxRowNum);
        if (array.length < minRowNum) {
            logger.error("{} 文件列数非法 {} [{}]", currentSimpleClassName, array.length, line);
            throw new JavaCG2RuntimeException(currentSimpleClassName + "文件列数非法");
        }

        return array;
    }

    /**
     * 执行开始之前的操作
     */
    public void beforeHandle(String javaCG2OutputPath) throws FileNotFoundException {
        if (writeFile) {
            String outputFilePath = javaCG2OutputPath + writeFileName;
            logger.info("{} 将结果写入文件 {}", currentSimpleClassName, outputFilePath);
            fileWriter = JavaCG2FileUtil.genBufferedWriter(outputFilePath);
        }
    }

    /**
     * 执行完成之后的操作
     */
    public void afterHandle() {
        // 结束前将剩余数据写入数据库
        insertDb();

        // 记录当前写入的数据库表枚举名
        recordWrittenDbTableEnum();

        if (writeFile && fileWriter != null) {
            IOUtils.closeQuietly(fileWriter);
        }
    }

    /**
     * 最后的检查
     *
     * @return true: 通过 false: 未通过
     */
    public boolean finalCheck() {
        if (!dataList.isEmpty()) {
            logger.error("{} 存在 {} 条数据还未入数据库，在结束前需要调用 afterHandle 方法写入数据库", currentSimpleClassName, dataList.size());
            return false;
        }
        return true;
    }

    private void checkInited() {
        if (initChecked) {
            return;
        }
        if (!inited) {
            logger.error("需要先调用 init 方法后再调用当前方法");
            throw new JavaCG2RuntimeException("需要先调用 init 方法后再调用当前方法");
        }
        if (!skipDependsCheck && !useNeo4j()) {
            // 检查依赖的其他数据库表有没有先写入
            DbTableInfoEnum[] dependsWriteDbTableEnums = jacgWriteDbHandler.dependsWriteDbTableEnums();
            if (ArrayUtils.isNotEmpty(dependsWriteDbTableEnums)) {
                for (DbTableInfoEnum dbTableInfoEnum : dependsWriteDbTableEnums) {
                    String enumName = dbTableInfoEnum.name();
                    if (!writeDbResult.getWrittenDbTableEnumNameSet().contains(enumName)) {
                        logger.error("当前类依赖的数据库表还未写入 {} {}", currentSimpleClassName, enumName);
                        throw new JavaCG2RuntimeException("当前类依赖的数据库表还未写入 " + currentSimpleClassName + " " + enumName);
                    }
                }
            }
        }
        initChecked = true;
    }

    /**
     * 读取文件并写入数据库
     *
     * @param javaCG2OutputInfo
     * @return
     */
    public boolean handle(JavaCG2OutputInfo javaCG2OutputInfo) {
        checkInited();

        String filePath;
        if (!mainFile) {
            filePath = javaCG2OutputInfo.getOtherFilePath(otherFileName);
            if (filePath == null) {
                logger.info("{} 当前文件未生成，不处理 {}", currentSimpleClassName, otherFileName);
                // 记录当前写入的数据库表枚举名
                recordWrittenDbTableEnum();
                return true;
            }
        } else {
            filePath = javaCG2OutputInfo.getMainFilePath(mainFileTypeEnum);
        }

        logger.info("{} 开始处理文件 {}", currentSimpleClassName, filePath);
        try (BufferedReader br = JavaCG2FileUtil.genBufferedReader(filePath)) {
            // 执行开始之前的操作
            beforeHandle(javaCG2OutputInfo.getOutputDirPath());

            String line;
            while ((line = br.readLine()) != null) {
                if (StringUtils.isBlank(line)) {
                    continue;
                }

                if (minColumnNum == maxColumnNum && !allowTabInLastColumn) {
                    lineArray = splitEquals(line, minColumnNum);
                } else {
                    lineArray = splitBetween(line, minColumnNum, maxColumnNum);
                }

                lineArrayIndex = 0;
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
            logger.error("出现异常 {} ", currentSimpleClassName, e);
            return false;
        } finally {
            // 执行完成之后的操作
            afterHandle();
        }
    }

    // 记录当前写入的数据库表枚举名
    private void recordWrittenDbTableEnum() {
        writeDbResult.getWrittenDbTableEnumNameSet().add(jacgWriteDbHandler.dbTableInfoEnum().name());
    }

    /**
     * 尝试将数据写入数据库
     */
    public void tryInsertDb() {
        if (dataList.size() >= dbInsertBatchSize) {
            insertDb();
        }
    }

    /**
     * 将数据写入数据库
     */
    public void insertDb() {
        checkInited();

        if (dataList.isEmpty()) {
            return;
        }

        if (writeDbNum == null) {
            logger.error("记录写入数据库记录数量的对象未生成，需要先调用 beforeHandle 方法创建 {}", currentSimpleClassName);
            throw new JavaCG2RuntimeException("记录写入数据库记录数量的对象未生成，需要先调用 beforeHandle 方法创建");
        }
        int writtenNum = writeDbNum.addAndGet(dataList.size());
        if (writtenNum % 50000 == 0) {
            logger.info("{} 写入数据库数量达到 {}", dbTableInfoEnum.getTableNameKeyword(), writtenNum);
        }

        if (writeFile) {
            if (writeFileNum == null) {
                logger.error("记录写入文件记录数量的对象未生成，需要先调用 beforeHandle 方法创建 {}", currentSimpleClassName);
                throw new JavaCG2RuntimeException("记录写入文件记录数量的对象未生成，需要先调用 beforeHandle 方法创建");
            }
            writeFileNum.addAndGet(dataList.size());
        }
        if (logger.isDebugEnabled()) {
            logger.debug("写入数据库 {} {}", currentSimpleClassName, dataList.size());
        }

        if (writeFile && fileWriter == null) {
            logger.error("写入文件的对象未生成，需要先调用 beforeHandle 方法创建 {}", currentSimpleClassName);
            failNum.addAndGet();
            throw new JavaCG2RuntimeException("写入文件的对象未生成，需要先调用 beforeHandle 方法创建");
        }

        // 等待直到允许任务执行
        JavaCG2ThreadUtil.wait4TPEAllowExecute(currentSimpleClassName, threadPoolExecutor, taskQueueMaxSize);

        if (!useNeo4j() || writeFile) {
            // 不使用neo4j，或需要写文件时，处理相关数据
            // 根据需要写入的数据生成Object数组
            List<Object[]> objectList = new ArrayList<>(dataList.size());
            for (T data : dataList) {
                // genObjectArray()方法在当前线程中执行，没有线程安全问题
                Object[] objects = genObjectArray(data);
                if (writeFile) {
                    try {
                        fileWriter.write(StringUtils.join(objects, JavaCG2Constants.FLAG_TAB) + JavaCG2Constants.NEW_LINE);
                    } catch (Exception e) {
                        logger.error("写文件出现异常 {} ", currentSimpleClassName, e);
                    }
                }
                objectList.add(objects);
            }
            if (!useNeo4j()) {
                // 不使用neo4j时，执行插入操作
                doInsertDb(objectList);
            }
        }
        if (useNeo4j()) {
            // 使用neo4j时，执行插入操作
            doInsertNeo4j();
        }
        dataList.clear();
    }

    private void doInsertDb(List<Object[]> objectList) {
        if (dbOperator == null) {
            logger.error("dbOperator为空 {}", currentSimpleClassName);
            throw new JavaCG2RuntimeException("dbOperator为空");
        }

        // 生成用于插入数据的sql语句
        String sql = dbOperWrapper.genAndCacheInsertSql(dbTableInfoEnum, DbInsertMode.DIME_INSERT);

        JavaCG2ThreadUtil.executeByTPE(currentSimpleClassName, threadPoolExecutor, runningTaskNum, () -> {
            // 批量写入数据库
            try {
                if (!dbOperator.batchInsert(sql, objectList)) {
                    failNum.addAndGet();
                }
            } catch (JACGSQLException e) {
                logger.error("{} 插入数据出现异常", currentSimpleClassName);
                failNum.addAndGet();
            }
        });
    }

    private void doInsertNeo4j() {
        List<T> usedDataList = new ArrayList<>(dataList);

        // 在线程池中执行插入neo4j数据操作
        JavaCG2ThreadUtil.executeByTPE(currentSimpleClassName, threadPoolExecutor, runningTaskNum, () -> {
            try {
                if (handleNeo4jDataList(usedDataList)) {
                    // 写入neo4j的数据的自定义处理
                    return;
                }

                // 写入neo4j数据不执行自定义处理，执行统一的处理
                List<Object> neo4jDomainList = new ArrayList<>(usedDataList.size());
                for (T data : usedDataList) {
                    Object neo4jDomain = transferNeo4jDomain(data);
                    neo4jDomainList.add(neo4jDomain);
                }
                JACGNeo4jUtil.saveAll(neo4jRepository, neo4jDomainList);
            } catch (Exception e) {
                logger.error("出现异常 {} ", currentSimpleClassName, e);
                failNum.addAndGet();
            }
        });
    }

    /**
     * 处理需要写入neo4j的数据的自定义处理
     *
     * @param dataList
     * @return true: 进行自定义处理 false: 不进行自定义处理
     */
    protected boolean handleNeo4jDataList(List<T> dataList) {
        return false;
    }

    /**
     * 返回读文件写数据库是否失败
     *
     * @return false: 未失败 true: 失败
     */
    public boolean checkFailed() {
        return ArrayUtils.isNotEmpty(dbTableInfoEnum.getColumns()) && failNum.getCount() > 0;
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
    public int genNextRecordId() {
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
        if ((getMethod && !JavaCG2ClassMethodUtil.matchesGetMethod(getSetMethodName)) || (!getMethod && !JavaCG2ClassMethodUtil.matchesSetMethod(getSetMethodName))) {
            // 根据方法名判断不是get/set方法
            return false;
        }

        String currentSimpleClassName = simpleClassName;
        while (true) {
            // 判断当前类的get/set方法是否是dto的get/set方法
            logger.debug("判断当前类的get/set方法是否是dto的get/set方法 {}", currentSimpleClassName);
            Set<String> methodSet = getSetMethodSimpleClassMap.get(currentSimpleClassName);
            if (methodSet != null && methodSet.contains(getSetMethodName)) {
                return true;
            }
            // 获取当前类的父类
            String superSimpleClassName = extendsSimpleClassNameMap.get(currentSimpleClassName);
            if (superSimpleClassName == null) {
                // 当前类未找到父类
                return false;
            }
            if (superSimpleClassName.equals(currentSimpleClassName)) {
                logger.error("当前类找到的父类唯一类名相同 {}", currentSimpleClassName);
                throw new JavaCG2RuntimeException("当前类找到的父类唯一类名相同 " + currentSimpleClassName);
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

    public void setAppName(String appName) {
        this.appName = appName;
    }

    public void setThreadPoolExecutor(ThreadPoolExecutor threadPoolExecutor) {
        this.threadPoolExecutor = threadPoolExecutor;
    }

    public void setRunningTaskNum(AtomicInteger runningTaskNum) {
        this.runningTaskNum = runningTaskNum;
    }

    public void setTaskQueueMaxSize(int taskQueueMaxSize) {
        this.taskQueueMaxSize = taskQueueMaxSize;
    }

    public void setSkipDependsCheck(boolean skipDependsCheck) {
        this.skipDependsCheck = skipDependsCheck;
    }
}
