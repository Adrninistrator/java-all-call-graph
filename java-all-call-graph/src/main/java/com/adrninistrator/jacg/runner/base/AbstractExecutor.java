package com.adrninistrator.jacg.runner.base;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.conf.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg2.thread.ThreadFactory4TPE;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

/**
 * @author adrninistrator
 * @date 2024/7/12
 * @description: 任务线程池执行任务的基类
 */
public abstract class AbstractExecutor {
    private static final Logger logger = LoggerFactory.getLogger(AbstractExecutor.class);

    protected final String currentSimpleClassName = this.getClass().getSimpleName();

    // 配置信息包装类
    protected ConfigureWrapper configureWrapper;

    protected ThreadFactory4TPE threadFactory4TPE;

    protected ThreadPoolExecutor threadPoolExecutor;

    // 任务队列最大长度
    protected int taskQueueMaxSize;

    /**
     * 创建线程池
     *
     * @param taskNum 任务数量，非空时尝试根据任务数量调中实际创建的线程数
     */
    protected void createThreadPoolExecutor(Integer taskNum) {
        int threadNum = configureWrapper.getMainConfig(ConfigKeyEnum.CKE_THREAD_NUM);
        if (taskNum != null && taskNum < threadNum) {
            // 任务数量比配置文件中指定的线程数少，则调小实际创建的线程数
            threadNum = taskNum;
        }

        // 任务队列最大长度，设置为线程数2倍
        taskQueueMaxSize = threadNum * 2;
        logger.info("任务数量 {} 创建的线程池线程数 {}", (taskNum == null ? "-" : taskNum), threadNum);
        threadFactory4TPE = new ThreadFactory4TPE(JACGConstants.THREAD_NAME_PREFIX_WORKER);
        threadPoolExecutor = new ThreadPoolExecutor(threadNum, threadNum, 10L, TimeUnit.SECONDS,
                new LinkedBlockingQueue<>(taskQueueMaxSize), threadFactory4TPE);
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

    /**
     * 等待直到允许任务执行
     */
    protected void wait4TPEExecute() {
        JACGUtil.wait4TPEExecute(threadPoolExecutor, taskQueueMaxSize);
    }
}
