package com.adrninistrator.jacg.runner.base;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.conf.ConfInfo;
import com.adrninistrator.jacg.conf.ConfManager;
import com.adrninistrator.jacg.dboper.DbOperator;
import com.adrninistrator.jacg.thread.ThreadFactory4TPE;
import com.adrninistrator.jacg.util.FileUtil;
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
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
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

    // 退出前是否关闭数据源，默认为是
    protected static boolean CLOSE_DS_BEFORE_EXIT = true;

    // 是否有检查过数据库文件是否可写
    protected static boolean CHECK_H2_DB_FILE_WRITEABLE = false;

    protected static AbstractRunner runner;

    protected ConfInfo confInfo;

    protected DbOperator dbOperator;

    protected ThreadPoolExecutor threadPoolExecutor;

    // 预编译SQL语句缓存
    protected Map<String, String> sqlCacheMap = new ConcurrentHashMap<>();

    // 任务执行失败标志
    protected boolean someTaskFail = false;

    // 记录执行失败的任务信息
    protected List<String> failTaskList = new ArrayList<>();

    // 任务队列最大长度
    protected int taskQueueMaxSize;

    public static void main(String[] args) {
        runner.run();
    }

    /**
     * 执行任务
     *
     * @return true: 成功；false: 失败
     */
    public boolean run() {
        long startTime = System.currentTimeMillis();
        someTaskFail = false;

        confInfo = ConfManager.getConfInfo();
        if (confInfo == null) {
            return false;
        }

        if (!preCheck()) {
            logger.error("{} 预检查失败", this.getClass().getSimpleName());
            return false;
        }

        dbOperator = DbOperator.getInstance();
        if (!dbOperator.init(confInfo)) {
            return false;
        }

        if (!init()) {
            logger.error("{} 初始化失败", this.getClass().getSimpleName());
            return false;
        }

        operate();

        beforeExit();

        long spendTime = System.currentTimeMillis() - startTime;
        logger.info("{} 耗时: {} s", this.getClass().getSimpleName(), spendTime / 1000.0D);

        return !someTaskFail;
    }

    /**
     * 预检查
     *
     * @return true: 成功；false: 失败
     */
    public abstract boolean preCheck();

    /**
     * 初始化
     *
     * @return true: 成功；false: 失败
     */
    public abstract boolean init();

    /**
     * 执行操作
     */
    public abstract void operate();

    protected void beforeExit() {
        if (someTaskFail) {
            logger.error("有任务执行失败，请检查\n{}", StringUtils.join(failTaskList, "\n"));
        } else {
            logger.info("任务执行完毕");
        }

        if (threadPoolExecutor != null) {
            threadPoolExecutor.shutdown();
        }

        if (CLOSE_DS_BEFORE_EXIT) {
            logger.info("操作结束时关闭数据源");
            dbOperator.closeDs();
        } else {
            logger.info("操作结束时不关闭数据源");
        }
    }

    protected void cacheSql(String key, String sql) {
        if (sqlCacheMap.putIfAbsent(key, sql) == null) {
            logger.info("cache sql: [{}] [{}]", key, sql);
        }
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

    // 检查H2数据库文件是否可写
    protected boolean checkH2DbFileWritable(File h2DbFile) {
        // 以下操作在JVM中只能成功执行一次，需要避免执行多次
        if (CHECK_H2_DB_FILE_WRITEABLE) {
            return true;
        }

        // 尝试以写方式打开，检查数据库文件是否被占用
        try (FileChannel channel = FileChannel.open(h2DbFile.toPath(), StandardOpenOption.WRITE)) {
            FileLock fileLock = channel.tryLock();
            if (fileLock == null) {
                logger.error("H2数据库文件无法写入，请先关闭H2数据库工具打开的H2数据库文件 {}", FileUtil.getCanonicalPath(h2DbFile));
                return false;
            }

            fileLock.release();

            CHECK_H2_DB_FILE_WRITEABLE = true;
            return true;
        } catch (Exception e) {
            logger.error("检查H2数据库文件是否可以写入失败 {} ", FileUtil.getCanonicalPath(h2DbFile), e);
            return false;
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

    public static void setCloseDsBeforeExit(boolean closeDsBeforeExit) {
        CLOSE_DS_BEFORE_EXIT = closeDsBeforeExit;
    }
}
