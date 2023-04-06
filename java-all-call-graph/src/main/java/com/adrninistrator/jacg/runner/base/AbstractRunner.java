package com.adrninistrator.jacg.runner.base;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.ConfigDbKeyEnum;
import com.adrninistrator.jacg.common.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.common.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dboper.DbOperator;
import com.adrninistrator.jacg.handler.extends_impl.JACGExtendsImplHandler;
import com.adrninistrator.jacg.thread.ThreadFactory4TPE;
import com.adrninistrator.jacg.util.JACGFileUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg.util.JavaCGUtil;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.nio.channels.OverlappingFileLockException;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

/**
 * @author adrninistrator
 * @date 2021/6/17
 * @description:
 */
public abstract class AbstractRunner {
    private static final Logger logger = LoggerFactory.getLogger(AbstractRunner.class);

    // 配置信息包装类
    protected ConfigureWrapper configureWrapper;

    // 当前的输出目录
    protected String currentOutputDirPath;

    protected String appName;

    // 是否有检查过数据库文件是否可写
    protected static boolean CHECK_H2_DB_FILE_WRITEABLE = false;

    protected boolean inited = false;

    protected DbOperator dbOperator;

    protected DbOperWrapper dbOperWrapper;

    protected ThreadPoolExecutor threadPoolExecutor;

    // 任务执行失败标志
    protected boolean someTaskFail = false;

    // 记录执行失败的任务信息
    protected List<String> failTaskList = new ArrayList<>();

    // 任务队列最大长度
    protected int taskQueueMaxSize;

    // 继承与实际相关的处理类
    protected JACGExtendsImplHandler jacgExtendsImplHandler;

    protected final String currentSimpleClassName = this.getClass().getSimpleName();

    /**
     * 执行任务
     *
     * @return true: 成功；false: 失败
     */
    public boolean run() {
        return run(new ConfigureWrapper(false));
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
     * 初始化
     *
     * @param configureWrapper
     * @return
     */
    private boolean init(ConfigureWrapper configureWrapper) {
        synchronized (this) {
            if (inited) {
                logger.warn("{} 已完成初始化，不会再初始化", currentSimpleClassName);
                return true;
            }

            this.configureWrapper = configureWrapper;
            if (handleDb()) {
                // 需要操作数据库时执行的操作
                appName = configureWrapper.getMainConfig(ConfigKeyEnum.CKE_APP_NAME);
                // 完成需要使用的基础配置的初始化
                dbOperWrapper = DbOperWrapper.genInstance(configureWrapper, currentSimpleClassName);
                dbOperator = dbOperWrapper.getDbOperator();
                jacgExtendsImplHandler = new JACGExtendsImplHandler(dbOperWrapper);
            }
            inited = true;
            return true;
        }
    }

    /**
     * 打印当前使用的配置信息
     */
    protected void printAllConfigInfo() {
        String configMdFilePath = JavaCGUtil.addSeparator4FilePath(currentOutputDirPath) + JACGConstants.FILE_JACG_ALL_CONFIG_MD;
        logger.info("{} 全部的配置参数信息保存到以下文件 {}", currentSimpleClassName, configMdFilePath);
        // 打印所有的配置参数信息
        configureWrapper.printConfigInfo(currentSimpleClassName, configMdFilePath, true);
    }

    /**
     * 执行任务，通过代码指定配置参数
     *
     * @param configureWrapper 当前使用的配置信息
     * @return
     */
    public boolean run(ConfigureWrapper configureWrapper) {
        // 记录入口简单类名
        configureWrapper.addEntryClass(currentSimpleClassName);

        try {
            logger.info("{} 开始执行", currentSimpleClassName);
            long startTime = System.currentTimeMillis();
            someTaskFail = false;

            // 初始化
            if (!init(configureWrapper)) {
                logger.error("{} 初始化失败", currentSimpleClassName);
                return false;
            }

            // 预检查
            if (!preCheck()) {
                logger.error("{} 预检查失败", currentSimpleClassName);
                return false;
            }

            // 预处理
            if (!preHandle()) {
                logger.error("{} 预处理失败", currentSimpleClassName);
                return false;
            }

            // 执行处理
            handle();

            if (someTaskFail) {
                logger.error("{} 执行失败", currentSimpleClassName);
                return false;
            }
            // 执行完毕时尝试打印当前使用的配置信息
            configureWrapper.tryPrintUsedConfigInfo(currentSimpleClassName, currentOutputDirPath);
            long spendTime = System.currentTimeMillis() - startTime;
            logger.info("{} 执行完毕，耗时: {} S", currentSimpleClassName, spendTime / 1000.0D);
            return true;
        } catch (Exception e) {
            logger.error("error {} ", currentSimpleClassName, e);
            return false;
        } finally {
            // 结束前的处理，需要确保能执行到，在其中会关闭数据源
            beforeExit();
        }
    }

    /**
     * 预检查
     *
     * @return true: 成功；false: 失败
     */
    protected boolean preCheck() {
        if (handleDb()) {
            // 需要操作数据库时执行的操作
            if (dbOperator.isClosed()) {
                logger.error("当前类上次执行方法完毕后已将数据库关闭，若需要再次执行，需要重新创建对象再执行");
                return false;
            }

            // 使用H2数据库时，检查数据库文件
            if (Boolean.TRUE.equals(configureWrapper.getMainConfig(ConfigDbKeyEnum.CDKE_DB_USE_H2)) && !checkH2DbFile()) {
                return false;
            }
        }

        return true;
    }

    // 结束前的处理
    protected void beforeExit() {
        if (dbOperator != null) {
            dbOperator.closeDs();
        }

        if (threadPoolExecutor != null) {
            threadPoolExecutor.shutdown();
        }

        if (someTaskFail) {
            logger.error("{} 有任务执行失败，请检查\n{}", currentSimpleClassName, StringUtils.join(failTaskList, "\n"));
        } else {
            logger.info("{} 任务执行完毕", currentSimpleClassName);
        }
    }

    /**
     * 创建线程池
     *
     * @param taskNum 任务数量，非空时尝试根据任务数量调中实际创建的线程数
     */
    protected void createThreadPoolExecutor(Integer taskNum) {
        int threadNum = configureWrapper.getMainConfig(ConfigKeyEnum.CKE_THREAD_NUM);
        if (taskNum != null && taskNum < threadNum) {
            // 任务数量比配置文件中指定的线程数少，则调小实际创建的线程数
            logger.info("{} 将线程数修改为需要处理的任务数 {}", currentSimpleClassName, taskNum);
            configureWrapper.setMainConfig(ConfigKeyEnum.CKE_THREAD_NUM, String.valueOf(taskNum));
        }

        // 任务队列最大长度，设置为线程数2倍
        taskQueueMaxSize = threadNum * 2;
        threadPoolExecutor = new ThreadPoolExecutor(threadNum, threadNum, 10L, TimeUnit.SECONDS,
                new LinkedBlockingQueue<>(taskQueueMaxSize), new ThreadFactory4TPE(JACGConstants.THREAD_NAME_PREFIX_WORKER));
    }

    // 等待直到任务执行完毕
    protected void wait4TPEDone() {
        while (true) {
            if (threadPoolExecutor.getActiveCount() == 0 && threadPoolExecutor.getQueue().isEmpty()) {
                return;
            }
            logger.debug("{} wait4TPEDone ...", currentSimpleClassName);
            JACGUtil.sleep(100L);
        }
    }

    // 获取H2数据库文件对象
    protected File getH2DbFile() {
        return new File(configureWrapper.getMainConfig(ConfigDbKeyEnum.CDKE_DB_H2_FILE_PATH) + JACGConstants.H2_FILE_EXT);
    }

    // 获得需要处理的jar包数组
    protected List<String> getJarPathList() {
        List<String> jarPathList = configureWrapper.getOtherConfigList(OtherConfigFileUseListEnum.OCFULE_JAR_DIR, true);
        logger.info("{} 需要处理的jar包或目录\n{}", currentSimpleClassName, StringUtils.join(jarPathList, "\n"));
        return jarPathList;
    }

    /**
     * 检查H2数据库文件是否可写
     * 需要进行同步控制，避免同时执行
     *
     * @param h2DbFile
     * @return
     */
    protected boolean checkH2DbFileWritable(File h2DbFile) {
        synchronized (AbstractRunner.class) {
            // 以下操作在JVM中只能成功执行一次，需要避免执行多次
            if (CHECK_H2_DB_FILE_WRITEABLE) {
                return true;
            }

            logger.info("{} 检查H2数据库文件是否可写", currentSimpleClassName);
            // 尝试以写方式打开，检查数据库文件是否被占用
            try (FileChannel channel = FileChannel.open(h2DbFile.toPath(), StandardOpenOption.WRITE)) {
                FileLock fileLock = channel.tryLock();
                if (fileLock == null) {
                    logger.error("{} H2数据库文件无法写入，请先关闭H2数据库工具打开的H2数据库文件 {}", currentSimpleClassName, JACGFileUtil.getCanonicalPath(h2DbFile));
                    return false;
                }

                fileLock.release();
                logger.info("{} H2数据库文件可写", currentSimpleClassName);
                CHECK_H2_DB_FILE_WRITEABLE = true;
                return true;
            } catch (OverlappingFileLockException e) {
                // 某些情况下会出现以上异常，当作正常处理
                logger.warn("{} 检查H2数据库文件是否可以写入时，出现重复对文件加锁的异常，当作正常处理 {} {} {}", currentSimpleClassName, JACGFileUtil.getCanonicalPath(h2DbFile),
                        e.getClass().getName(), e.getMessage());
                return true;
            } catch (Exception e) {
                logger.error("{} 检查H2数据库文件是否可以写入失败，请重试 {} ", currentSimpleClassName, JACGFileUtil.getCanonicalPath(h2DbFile), e);
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
        return true;
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
