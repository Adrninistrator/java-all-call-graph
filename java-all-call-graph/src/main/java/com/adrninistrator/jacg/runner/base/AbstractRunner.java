package com.adrninistrator.jacg.runner.base;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.conf.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.dboper.DbInitializer;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dboper.DbOperator;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4JarInfo;
import com.adrninistrator.jacg.handler.extendsimpl.JACGExtendsImplHandler;
import com.adrninistrator.jacg.handler.jarinfo.JarInfoHandler;
import com.adrninistrator.jacg.neo4j.handler.extendsimpl.Neo4jExtendsImplHandler;
import com.adrninistrator.jacg.runner.RunnerWriteDb;
import com.adrninistrator.jacg.util.JACGFileUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.conf.enums.JavaCG2OtherConfigFileUseListEnum;
import com.adrninistrator.javacg2.util.JavaCG2FileUtil;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import com.alibaba.druid.pool.DruidDataSource;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.nio.channels.OverlappingFileLockException;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * @author adrninistrator
 * @date 2021/6/17
 * @description: 执行类基类
 */
public abstract class AbstractRunner extends AbstractExecutor {
    private static final Logger logger = LoggerFactory.getLogger(AbstractRunner.class);

    public static final String ERROR_LOG_NOTICE = "请在日志中搜索“ ERROR ”查看对应失败原因";

    private final AtomicBoolean runFlag = new AtomicBoolean(false);

    protected DbOperator dbOperator;

    protected DbOperWrapper dbOperWrapper;

    protected String appName;

    protected String tableSuffix;

    // 任务执行失败标志
    protected boolean someTaskFail = false;

    // 记录执行失败的任务信息
    protected List<String> failTaskList = new ArrayList<>();

    // 继承与实际相关的处理类
    protected JACGExtendsImplHandler jacgExtendsImplHandler;

    // 对解析的jar包及目录信息处理的类
    protected JarInfoHandler jarInfoHandler;

    public AbstractRunner() {
        this(new ConfigureWrapper(false));
    }

    public AbstractRunner(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
        // 对当前使用的配置进行自定义设置，适用于在构造函数中需要修改的场景
        configConfigureWrapper(this.configureWrapper);

        appName = this.configureWrapper.getMainConfig(ConfigKeyEnum.CKE_APP_NAME);
        // 判断当前的子类是否需要操作数据库，RunnerWriteCallGraphFile 类不需要
        if (useNeo4j()) {
            dbOperWrapper = DbInitializer.genDbOperWrapper(this.configureWrapper, true, this);
            jacgExtendsImplHandler = new Neo4jExtendsImplHandler(this.configureWrapper);
        } else if (handleDb()) {
            // 完成需要使用的基础配置的初始化
            dbOperWrapper = DbInitializer.genDbOperWrapper(this.configureWrapper, false, this);
            dbOperator = dbOperWrapper.getDbOperator();
            tableSuffix = dbOperator.getTableSuffix();
            jacgExtendsImplHandler = new JACGExtendsImplHandler(dbOperWrapper);
            jarInfoHandler = new JarInfoHandler(dbOperWrapper);
        }
    }

    /**
     * 对当前使用的配置进行自定义设置，适用于在构造函数中需要修改的场景
     *
     * @param configureWrapper
     */
    protected void configConfigureWrapper(ConfigureWrapper configureWrapper) {
    }

    /**
     * 初始化
     *
     * @return true: 成功；false: 失败
     */
    protected abstract boolean preHandle();

    /**
     * 执行处理
     */
    protected abstract void handle();

    /**
     * 检查H2数据库文件
     *
     * @return true: 检查通过 false: 检查不通过
     */
    protected abstract boolean checkH2DbFile();

    /**
     * 打印所有的配置参数
     */
    protected boolean printAllConfigInfo() {
        return configureWrapper.printAllConfigInfo(currentSimpleClassName, currentOutputDirPath, JACGConstants.FILE_JACG_ALL_CONFIG_MD);
    }

    /**
     * 打印有使用的配置参数
     */
    protected boolean printUsedConfigInfo() {
        return configureWrapper.printUsedConfigInfo(currentSimpleClassName, currentOutputDirPath, JACGConstants.FILE_JACG_USED_CONFIG_MD);
    }

    /**
     * 入口方法，通过代码指定配置参数
     *
     * @return
     */
    public boolean run() {
        if (!runFlag.compareAndSet(false, true)) {
            logger.error("当前类的实例在创建后只能使用一次，假如需要再次执行，请创建新的实例 {}", currentSimpleClassName);
            return false;
        }

        try {
            logger.info("{} 开始执行", currentSimpleClassName);
            long startTime = System.currentTimeMillis();
            someTaskFail = false;

            // 预检查
            if (!preCheck()) {
                logger.error("{} 预检查失败 {}", currentSimpleClassName, ERROR_LOG_NOTICE);
                return false;
            }

            // 预处理
            if (!preHandle()) {
                logger.error("{} 预处理失败 {}", currentSimpleClassName, ERROR_LOG_NOTICE);
                return false;
            }

            // 执行处理
            handle();

            if (threadFactory4TPE != null) {
                int threadPoolExceptionCount = threadFactory4TPE.getExceptionCount();
                if (threadPoolExceptionCount > 0) {
                    logger.error("线程池执行的任务失败次数为 {} {}", currentSimpleClassName, threadPoolExceptionCount);
                    someTaskFail = true;
                }
            }

            if (someTaskFail) {
                logger.error("{} 执行失败 {}", currentSimpleClassName, ERROR_LOG_NOTICE);
                return false;
            }

            if (currentOutputDirPath != null) {
                // 打印当前使用的配置参数
                if (!printUsedConfigInfo()) {
                    return false;
                }
            } else {
                logger.info("没有执行生成文件的操作，不打印当前使用的配置参数 {}", currentSimpleClassName);
            }

            logger.info("{} 执行完毕，当前生成文件所在目录 {} ，耗时: {} 秒", currentSimpleClassName, currentOutputDirPath, JavaCG2Util.getSpendSeconds(startTime));
            return true;
        } catch (Exception e) {
            logger.error("出现异常 {} {} ", currentSimpleClassName, ERROR_LOG_NOTICE, e);
            return false;
        } finally {
            // 结束前的处理，需要确保能执行到，在其中会关闭数据源
            beforeExit();
        }
    }

    // 检查使用的组件版本
    private void checkJarVersion() {
        doCheckJarVersion(DruidDataSource.class, "druid-1.2.15.jar");
    }

    // 执行检查使用的组件版本
    private void doCheckJarVersion(Class<?> clazz, String expectedJarName) {
        String jarFilePath = JavaCG2FileUtil.getJarFilePathOfClass(clazz);
        /*
            spring-boot中的jar格式如下，需要兼容：
            file:/D:/java-all-call-graph/build/spring-boot/java-all-call-graph.jar!/BOOT-INF/lib/druid-1.2.15.jar!/
         */
        String springJarTail = "!/";
        if (StringUtils.endsWith(jarFilePath, springJarTail)) {
            jarFilePath = jarFilePath.substring(0, jarFilePath.length() - springJarTail.length());
        }
        if (!StringUtils.endsWith(jarFilePath, expectedJarName)) {
            logger.error("{} 类所在的jar包 {} 与要求的组件名称或版本 {} 不一致，请检查对应组件版本与 java-all-call-graph 项目中使用的是否相同", clazz.getName(), jarFilePath, expectedJarName);
            return;
        }
        logger.info("{} 类所在的jar包 {} 与要求的组件名称或版本 {} 一致", clazz.getName(), jarFilePath, expectedJarName);
    }

    /**
     * 预检查
     *
     * @return true: 成功；false: 失败
     */
    protected boolean preCheck() {
        // 检查使用的组件版本
        checkJarVersion();

        if (handleDb()) {
            // 需要操作数据库时执行的操作
            if (dbOperator.isClosed()) {
                logger.error("{} 当前类上次执行方法完毕后已将数据库关闭，若需要再次执行，需要重新创建对象再执行", currentSimpleClassName);
                return false;
            }

            // 使用H2数据库时，检查数据库文件
            return !dbOperator.getDbConfInfo().isUseH2Db() || checkH2DbFile();
        }
        return true;
    }

    // 结束前的处理
    protected void beforeExit() {
        if (threadPoolExecutor != null) {
            // 关闭并等待线程池
            shutdownAndWaitTPE();
        }

        if (handleDb()) {
            dbOperator.closeDs(this);
        }

        if (someTaskFail) {
            logger.error("{} 有任务执行失败 {} 执行失败的任务\n{}", currentSimpleClassName, ERROR_LOG_NOTICE, StringUtils.join(failTaskList, "\n"));
        } else {
            logger.info("{} 任务执行完毕", currentSimpleClassName);
        }
    }

    // 获取H2数据库文件对象
    protected File getH2DbFile() {
        return new File(dbOperator.getDbConfInfo().getDbH2FilePath() + JACGConstants.H2_FILE_EXT);
    }

    /**
     * 检查H2数据库文件是否可写
     * 需要进行同步控制，避免同时执行
     *
     * @param allowNotExists 是否允许H2数据库文件不存在
     * @return
     */
    protected boolean checkH2DbFileWritable(boolean allowNotExists) {
        File h2DbFile = getH2DbFile();
        if (!h2DbFile.exists()) {
            if (!allowNotExists) {
                logger.error("H2数据库文件不存在，请先执行 {} 类导入数据库 {}", RunnerWriteDb.class.getSimpleName(), JavaCG2FileUtil.getCanonicalPath(h2DbFile));
                return false;
            }
            return true;
        }

        // 数据库文件存在
        if (!h2DbFile.isFile()) {
            logger.error("H2数据库文件不是文件 {}", JavaCG2FileUtil.getCanonicalPath(h2DbFile));
            return false;
        }

        synchronized (AbstractRunner.class) {
            logger.info("{} 检查H2数据库文件是否可写", currentSimpleClassName);
            // 尝试以写方式打开，检查数据库文件是否被占用
            try (FileChannel channel = FileChannel.open(h2DbFile.toPath(), StandardOpenOption.WRITE)) {
                FileLock fileLock = channel.tryLock();
                if (fileLock == null) {
                    logger.error("{} H2数据库文件无法写入，请先关闭H2数据库工具打开的H2数据库文件 {}", currentSimpleClassName, JavaCG2FileUtil.getCanonicalPath(h2DbFile));
                    return false;
                }

                fileLock.release();
                logger.info("{} H2数据库文件可写", currentSimpleClassName);
                return true;
            } catch (OverlappingFileLockException e) {
                // 某些情况下会出现以上异常，当作正常处理
                logger.warn("{} 检查H2数据库文件是否可以写入时，出现重复对文件加锁的异常，当作正常处理 {} {} {}", currentSimpleClassName, JavaCG2FileUtil.getCanonicalPath(h2DbFile),
                        e.getClass().getName(), e.getMessage());
                return true;
            } catch (Exception e) {
                logger.error("{} 检查H2数据库文件是否可以写入失败，请重试 {} ", currentSimpleClassName, JavaCG2FileUtil.getCanonicalPath(h2DbFile), e);
                return false;
            }
        }
    }

    // 记录执行失败
    protected void recordTaskFail() {
        someTaskFail = true;
    }

    // 记录执行失败的任务信息
    protected void recordTaskFail(String taskInfo) {
        someTaskFail = true;

        synchronized (AbstractRunner.class) {
            failTaskList.add(taskInfo);
        }
    }

    /**
     * 是否需要操作数据库
     *
     * @return true: 需要操作数据库 false: 不需要操作数据库
     */
    protected boolean handleDb() {
        return false;
    }

    /**
     * 是否使用neo4j
     *
     * @return true: 使用neo4j false: 不使用neo4j
     */
    protected boolean useNeo4j() {
        return false;
    }

    /**
     * 创建输出文件所在目录
     *
     * @param upwardDir 上层目录名
     * @return
     */
    protected boolean createOutputDir(String upwardDir) {
        return createOutputDir(upwardDir, null);
    }

    /**
     * 创建输出文件所在目录
     *
     * @param upwardDir   上层目录名
     * @param downwardDir 下层目录名，可为空
     * @return
     */
    protected boolean createOutputDir(String upwardDir, String downwardDir) {
        if (currentOutputDirPath != null) {
            logger.info("已在外部指定当前的输出目录 {}", currentOutputDirPath);
        } else {
            logger.info("未在外部指定当前的输出目录");
            String outputDirPrefix;
            String outputRootPathInProperties = configureWrapper.getMainConfig(ConfigKeyEnum.CKE_OUTPUT_ROOT_PATH);
            if (StringUtils.isNotBlank(outputRootPathInProperties)) {
                // 使用指定的生成结果文件根目录，并指定当前应用名称
                outputDirPrefix = JavaCG2FileUtil.addSeparator4FilePath(outputRootPathInProperties) + upwardDir + File.separator;
            } else {
                // 使用当前目录作为生成结果文件根目录
                outputDirPrefix = upwardDir + File.separator;
            }

            String outputDirName = configureWrapper.getMainConfig(ConfigKeyEnum.CKE_OUTPUT_DIR_NAME);
            if (StringUtils.isNotBlank(outputDirName)) {
                // 使用指定的名称作为目录名
                outputDirPrefix += outputDirName;
            } else {
                // 完整目录名使用{app.name}{output.dir.flag}_{当前时间}
                String outputDirFlag = configureWrapper.getMainConfig(ConfigKeyEnum.CKE_OUTPUT_DIR_FLAG);
                // 需要进行同步控制，避免创建同名目录
                synchronized (AbstractRunner.class) {
                    outputDirPrefix = outputDirPrefix + appName + outputDirFlag + JACGConstants.FLAG_UNDER_LINE + JavaCG2Util.currentTime();
                }
            }

            if (downwardDir != null) {
                outputDirPrefix = outputDirPrefix + File.separator + downwardDir;
            }

            File currentOutputDir = new File(outputDirPrefix);
            currentOutputDirPath = currentOutputDir.getAbsolutePath();
            logger.info("创建当前的输出目录 {}", currentOutputDirPath);

            if (StringUtils.isNotBlank(outputDirName) && currentOutputDir.exists()) {
                logger.error("输出目录已存在，若确实需要在该目录中输出，请先删除该目录 {} {} 对应配置参数为 {}", currentOutputDirPath, outputDirName,
                        ConfigKeyEnum.CKE_OUTPUT_DIR_NAME.genConfigUsage());
                return false;
            }
        }

        // 判断目录是否存在，不存在时尝试创建
        if (!JavaCG2FileUtil.isDirectoryExists(currentOutputDirPath)) {
            return false;
        }

        // 打印当前使用的配置参数
        printAllConfigInfo();
        return true;
    }

    // 查询jar包与目录信息
    protected Map<String, WriteDbData4JarInfo> queryJarFileInfo() {
        // 查询所有的jar包和目录信息
        List<WriteDbData4JarInfo> list = jarInfoHandler.queryAllJarInfo();
        if (JavaCG2Util.isCollectionEmpty(list)) {
            logger.warn("查询到jar文件信息为空");
            return null;
        }

        Map<String, WriteDbData4JarInfo> rtnMap = new HashMap<>(list.size());
        for (WriteDbData4JarInfo writeDbData4JarInfo : list) {
            if (!JavaCG2Constants.FILE_KEY_RESULT_DIR.equals(writeDbData4JarInfo.getJarType())) {
                // 跳过解析结果文件保存目录
                rtnMap.put(writeDbData4JarInfo.getJarPathHash(), writeDbData4JarInfo);
            }
        }
        return rtnMap;
    }

    /**
     * 检查配置文件与jar_info表中的jar文件是否不一致或出现变化
     *
     * @param jarPathList
     * @return true: 有变化 false: 没有变化
     */
    protected boolean checkSomeJarModified(List<String> jarPathList) {
        // 查询jar包与目录信息
        Map<String, WriteDbData4JarInfo> jarInfoMap = queryJarFileInfo();
        if (JavaCG2Util.isMapEmpty(jarInfoMap)) {
            logger.info("{} 表的内容为空", DbTableInfoEnum.DTIE_JAR_INFO.getTableNameKeyword());
            return true;
        }

        // 不检查配置文件中指定的jar包与数据库表中的数量，因为配置文件中可能指定目录，数据库表中保存的是目录中的jar包
        for (String jarPath : jarPathList) {
            String jarFilePath = JavaCG2FileUtil.getCanonicalPath(jarPath);
            if (jarFilePath == null) {
                logger.error("获取文件路径失败 {}", jarPath);
                return true;
            }

            String jarPathHash = JACGUtil.genHashWithLen(jarFilePath);
            WriteDbData4JarInfo jarInfo = jarInfoMap.get(jarPathHash);
            if (jarInfo == null) {
                logger.error("指定的jar包或目录未导入数据库中 {} {}", jarPath, jarFilePath);
                return true;
            }

            if (JavaCG2FileUtil.isFileExists(jarPath) && checkJarFileModified(jarFilePath, jarInfo)) {
                // 对文件检查HASH
                logger.info("指定的jar包文件内容有变化 {} {}", jarPath, jarFilePath);
                return true;
            }
        }
        String logContent = String.format("配置文件 %s 中指定的jar包都在 %s 表中且未发生变化", JavaCG2OtherConfigFileUseListEnum.OCFULE_JAR_DIR.getConfigPrintInfo(),
                DbTableInfoEnum.DTIE_JAR_INFO.getTableNameKeyword());
        logger.info("{}", logContent);
        return false;
    }

    /**
     * 检查jar_info表中的某个jar包是否有发生变化
     *
     * @param jarFilePath
     * @param jarInfo
     * @return true: 有变化 false: 没有变化
     */
    protected boolean checkJarFileModified(String jarFilePath, WriteDbData4JarInfo jarInfo) {
        String lastModifiedTime = JACGFileUtil.getFileLastModifiedTime(jarFilePath);
        if (lastModifiedTime.equals(jarInfo.getLastModifiedTime())) {
            logger.info("文件修改时间没有变化 {} {}", jarFilePath, lastModifiedTime);
            // 文件修改时间没有变化，返回没有变化
            return false;
        }
        String jarFileHash = JACGFileUtil.getFileMd5(jarFilePath);
        // 比较jar包文件HASH值，若与之前不同则返回有变化
        if (StringUtils.equals(jarFileHash, jarInfo.getJarFileHash())) {
            logger.info("文件HASH没有变化 {} {}", jarFilePath, jarFileHash);
            return false;
        }
        logger.info("文件HASH发生变化 {} {}->{}", jarFilePath, jarInfo.getJarFileHash(), jarFileHash);
        return true;
    }

    public boolean isSomeTaskFail() {
        return someTaskFail;
    }

    /**
     * 获取执行失败的任务信息
     *
     * @return
     */
    public List<String> getFailTaskList() {
        return failTaskList;
    }
}
