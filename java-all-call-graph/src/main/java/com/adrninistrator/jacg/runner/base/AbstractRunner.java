package com.adrninistrator.jacg.runner.base;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.conf.ConfInfo;
import com.adrninistrator.jacg.conf.ConfManager;
import com.adrninistrator.jacg.dboper.DbOperator;
import com.adrninistrator.jacg.thread.ThreadFactory4TPE;
import com.adrninistrator.jacg.util.FileUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.nio.file.StandardOpenOption;
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

    protected static AbstractRunner runner;

    protected ConfInfo confInfo;

    protected DbOperator dbOperator;

    protected ThreadPoolExecutor threadPoolExecutor;

    // 预编译SQL语句缓存
    protected Map<String, String> sqlCacheMap = new ConcurrentHashMap<>();

    // 有任务执行失败
    protected boolean someTaskFail = false;

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
            logger.error("有任务执行失败，请检查");
        } else {
            logger.info("任务执行完毕");
        }

        if (threadPoolExecutor != null) {
            threadPoolExecutor.shutdown();
        }

        dbOperator.closeDs();
    }

    protected void cacheSql(String key, String sql) {
        if (sqlCacheMap.putIfAbsent(key, sql) == null) {
            logger.info("cache sql: [{}] [{}]", key, sql);
        }
    }

    // 创建线程池
    protected void createThreadPoolExecutor() {
        threadPoolExecutor = new ThreadPoolExecutor(confInfo.getThreadNum(), confInfo.getThreadNum(), 30, TimeUnit.SECONDS,
                new LinkedBlockingQueue<>(JACGConstants.THREAD_POOL_MAX_QUEUE_SIZE), new ThreadFactory4TPE("worker"));
    }

    // 等待直到允许任务执行
    protected void wait4TPEExecute() {
        while (true) {
            if (threadPoolExecutor.getQueue().size() < JACGConstants.THREAD_POOL_WAIT_QUEUE_SIZE) {
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
        // 尝试以写方式打开，检查数据库文件是否被占用
        try (FileChannel channel = FileChannel.open(h2DbFile.toPath(), StandardOpenOption.WRITE)) {
            FileLock fileLock = channel.tryLock();
            if (fileLock == null) {
                logger.error("H2数据库文件无法写入，请先关闭H2数据库工具打开的H2数据库文件 {}", FileUtil.getCanonicalPath(h2DbFile));
                return false;
            }

            fileLock.release();
            return true;
        } catch (Exception e) {
            logger.error("检查H2数据库文件是否可以写入失败 {}", FileUtil.getCanonicalPath(h2DbFile));
            return false;
        }
    }
}
