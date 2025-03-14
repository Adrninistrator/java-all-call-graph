package com.adrninistrator.jacg.util;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * @author adrninistrator
 * @date 2025/3/6
 * @description:
 */
public class JACGThreadUtil {

    private static final Logger logger = LoggerFactory.getLogger(JACGThreadUtil.class);

    /**
     * 等待直到任务执行完毕
     *
     * @param currentSimpleClassName
     * @param threadPoolExecutor
     * @param runningTaskNum
     */
    public static void wait4TPEDone(String currentSimpleClassName, ThreadPoolExecutor threadPoolExecutor, AtomicInteger runningTaskNum) {
        while (true) {
            int queueSize = threadPoolExecutor.getQueue().size();
            int activeThreadCount = threadPoolExecutor.getActiveCount();
            int runningTaskNumValue = runningTaskNum.get();
            if (queueSize == 0 && activeThreadCount == 0 && runningTaskNumValue == 0) {
                if (logger.isDebugEnabled()) {
                    logger.debug("{} 线程池中的任务已执行完毕", currentSimpleClassName);
                }
                return;
            }
            if (logger.isDebugEnabled()) {
                logger.debug("{} 等待线程池任务执行完毕，任务队列数量 {} 活跃线程数 {} 正在执行的任务数量 {}", currentSimpleClassName, queueSize, activeThreadCount, runningTaskNumValue);
            }
            JACGUtil.sleep(100L);
        }
    }


    /**
     * 使用线程池执行任务
     *
     * @param currentSimpleClassName
     * @param threadPoolExecutor
     * @param runningTaskNum
     * @param runnable
     */
    public static void executeByTPE(String currentSimpleClassName, ThreadPoolExecutor threadPoolExecutor, AtomicInteger runningTaskNum, Runnable runnable) {
        threadPoolExecutor.execute(() -> {
            int currentRunningTaskNum = runningTaskNum.addAndGet(1);
            logger.debug("{} 开始执行任务 正在执行的任务数量 {}", currentSimpleClassName, currentRunningTaskNum);
            try {
                runnable.run();
            } finally {
                int currentRunningTaskNum2 = runningTaskNum.decrementAndGet();
                logger.debug("{} 结束执行任务 正在执行的任务数量 {}", currentSimpleClassName, currentRunningTaskNum2);
            }
        });
    }

    /**
     * 等待直到允许任务执行
     *
     * @param currentSimpleClassName
     * @param threadPoolExecutor
     * @param taskQueueMaxSize
     */
    public static void wait4TPEAllowExecute(String currentSimpleClassName, ThreadPoolExecutor threadPoolExecutor, int taskQueueMaxSize) {
        while (true) {
            if (threadPoolExecutor.getQueue().size() < taskQueueMaxSize) {
                return;
            }
            logger.debug("{} wait4TPEExecute ...", currentSimpleClassName);
            JACGUtil.sleep(100L);
        }
    }

    private JACGThreadUtil() {
        throw new IllegalStateException("illegal");
    }
}
