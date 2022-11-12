package com.adrninistrator.jacg.runner.base;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.conf.ConfInfo;
import com.adrninistrator.jacg.conf.ConfManager;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dboper.DbOperator;
import com.adrninistrator.jacg.thread.ThreadFactory4TPE;
import com.adrninistrator.jacg.util.JACGFileUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
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

    // 是否有检查过数据库文件是否可写
    protected static boolean CHECK_H2_DB_FILE_WRITEABLE = false;

    protected boolean inited = false;

    protected ConfInfo confInfo;

    protected DbOperator dbOperator;

    protected DbOperWrapper dbOperWrapper;

    protected ThreadPoolExecutor threadPoolExecutor;

    // 任务执行失败标志
    protected boolean someTaskFail = false;

    // 记录执行失败的任务信息
    protected List<String> failTaskList = new ArrayList<>();

    // 任务队列最大长度
    protected int taskQueueMaxSize;

    /**
     * 执行任务
     *
     * @return true: 成功；false: 失败
     */
    public boolean run() {
        return run(new ConfigureWrapper());
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
     * @return
     */
    protected abstract boolean checkH2DbFile();

    /**
     * 初始化
     *
     * @param configureWrapper
     * @return
     */
    public boolean init(ConfigureWrapper configureWrapper) {
        synchronized (this) {
            if (inited) {
                return true;
            }

            this.configureWrapper = configureWrapper;
            confInfo = ConfManager.getConfInfo(configureWrapper);
            if (confInfo == null) {
                return false;
            }

            dbOperator = DbOperator.genInstance(confInfo);
            if (dbOperator == null) {
                return false;
            }

            dbOperWrapper = new DbOperWrapper(dbOperator, confInfo.getAppName());
            inited = true;
            return true;
        }
    }

    /**
     * 执行任务，通过代码指定配置参数
     *
     * @param configureWrapper
     * @return
     */
    public boolean run(ConfigureWrapper configureWrapper) {
        long startTime = System.currentTimeMillis();
        someTaskFail = false;

        if (!init(configureWrapper)) {
            logger.error("{} 初始化失败", this.getClass().getSimpleName());
            return false;
        }

        if (!preCheck()) {
            logger.error("{} 预检查失败", this.getClass().getSimpleName());
            return false;
        }

        if (!preHandle()) {
            logger.error("{} 预处理失败", this.getClass().getSimpleName());
            return false;
        }

        // 执行处理
        handle();

        beforeExit();

        long spendTime = System.currentTimeMillis() - startTime;
        logger.info("{} 执行完毕，耗时: {} S", this.getClass().getSimpleName(), spendTime / 1000.0D);

        return !someTaskFail;
    }

    /**
     * 预检查
     *
     * @return true: 成功；false: 失败
     */
    protected boolean preCheck() {
        // 使用H2数据库时，检查数据库文件
        if (confInfo.isDbUseH2() && !checkH2DbFile()) {
            return false;
        }

        return true;
    }

    protected void beforeExit() {
        if (someTaskFail) {
            logger.error("有任务执行失败，请检查\n{}", StringUtils.join(failTaskList, "\n"));
        } else {
            logger.info("任务执行完毕");
        }

        if (threadPoolExecutor != null) {
            threadPoolExecutor.shutdown();
        }

        logger.info("操作结束时关闭数据源");
        dbOperator.closeDs();
    }

    /**
     * 创建线程池
     *
     * @param taskNum 任务数量，非空时尝试根据任务数量调中实际创建的线程数
     */
    protected void createThreadPoolExecutor(Integer taskNum) {
        if (taskNum != null && taskNum < confInfo.getThreadNum()) {
            // 任务数量比配置文件中指定的线程数少，则调小实际创建的线程数
            logger.info("将线程数修改为需要处理的任务数 {}", taskNum);
            confInfo.setThreadNum(taskNum);
        }

        // 任务队列最大长度，设置为线程数2倍
        taskQueueMaxSize = confInfo.getThreadNum() * 2;
        threadPoolExecutor = new ThreadPoolExecutor(confInfo.getThreadNum(), confInfo.getThreadNum(), 10L, TimeUnit.SECONDS,
                new LinkedBlockingQueue<>(taskQueueMaxSize), new ThreadFactory4TPE("jacg_worker"));
    }

    // 等待直到允许任务执行
    protected void wait4TPEExecute() {
        while (true) {
            if (threadPoolExecutor.getQueue().size() < taskQueueMaxSize) {
                return;
            }
            logger.debug("wait4TPEExecute ...");
            JACGUtil.sleep(100L);
        }
    }

    // 等待直到任务执行完毕
    protected void wait4TPEDone() {
        while (true) {
            if (threadPoolExecutor.getActiveCount() == 0 && threadPoolExecutor.getQueue().isEmpty()) {
                return;
            }
            logger.debug("wait4TPEDone ...");
            JACGUtil.sleep(100L);
        }
    }

    // 获取H2数据库文件对象
    protected File getH2DbFile() {
        return new File(confInfo.getDbH2FilePath() + JACGConstants.H2_FILE_EXT);
    }

    // 获得需要处理的jar包数组
    protected String[] getJarArray() {
        return confInfo.getCallGraphJarList().split(JACGConstants.FLAG_SPACE);
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

            logger.info("检查H2数据库文件是否可写");
            // 尝试以写方式打开，检查数据库文件是否被占用
            try (FileChannel channel = FileChannel.open(h2DbFile.toPath(), StandardOpenOption.WRITE)) {
                FileLock fileLock = channel.tryLock();
                if (fileLock == null) {
                    logger.error("H2数据库文件无法写入，请先关闭H2数据库工具打开的H2数据库文件 {}", JACGFileUtil.getCanonicalPath(h2DbFile));
                    return false;
                }

                fileLock.release();

                CHECK_H2_DB_FILE_WRITEABLE = true;
                return true;
            } catch (Exception e) {
                logger.error("检查H2数据库文件是否可以写入失败 {} ", JACGFileUtil.getCanonicalPath(h2DbFile), e);
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
     * 获取配置信息
     *
     * @return
     */
    public ConfInfo getConfInfo() {
        return confInfo;
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
