package com.adrninistrator.jacg.runner.base;

import com.adrninistrator.jacg.common.Constants;
import com.adrninistrator.jacg.conf.ConfInfo;
import com.adrninistrator.jacg.conf.ConfManager;
import com.adrninistrator.jacg.dboper.DbOperator;
import com.adrninistrator.jacg.thread.ThreadFactory4TPE;
import com.adrninistrator.jacg.util.CommonUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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

    public void run() {
        long startTime = System.currentTimeMillis();

        confInfo = ConfManager.getConfInfo();
        if (confInfo == null) {
            return;
        }

        dbOperator = DbOperator.getInstance();
        if (!dbOperator.init(confInfo)) {
            return;
        }

        if (!init()) {
            logger.error("{} 初始化失败", this.getClass().getSimpleName());
            return;
        }

        operate();

        beforeExit();

        long spendTime = System.currentTimeMillis() - startTime;
        logger.info("{} 执行完毕，耗时: {} s", this.getClass().getSimpleName(), spendTime / 1000.0D);
    }

    public abstract boolean init();

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
                new LinkedBlockingQueue<>(Constants.THREAD_POOL_MAX_QUEUE_SIZE), new ThreadFactory4TPE("worker"));
    }

    // 等待直到允许任务执行
    protected void wait4TPEExecute() {
        while (true) {
            if (threadPoolExecutor.getQueue().size() < Constants.THREAD_POOL_WAIT_QUEUE_SIZE) {
                return;
            }
            logger.debug("wait4TPEExecute ...");
            CommonUtil.sleep(100L);
        }
    }

    // 等待直到任务执行完毕
    protected void wait4TPEDone() {
        while (true) {
            if (threadPoolExecutor.getActiveCount() == 0 && threadPoolExecutor.getQueue().isEmpty()) {
                return;
            }
            logger.debug("wait4TPEDone ...");
            CommonUtil.sleep(100L);
        }
    }
}
