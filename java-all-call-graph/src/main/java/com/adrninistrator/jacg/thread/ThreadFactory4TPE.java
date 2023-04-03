package com.adrninistrator.jacg.thread;

import java.util.concurrent.ThreadFactory;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * @author adrninistrator
 * @date 2021/6/21
 * @description:
 */

public class ThreadFactory4TPE implements ThreadFactory {

    private static final AtomicInteger ai = new AtomicInteger(0);

    private final String threadNamePrefix;

    public ThreadFactory4TPE(String threadNamePrefix) {
        this.threadNamePrefix = threadNamePrefix;
    }

    @Override
    public Thread newThread(Runnable r) {
        Thread thread = new Thread(r);
        thread.setName(threadNamePrefix + "-" + ai.addAndGet(1));

        // 设置线程为非守护线程
        thread.setDaemon(false);

        return thread;
    }
}
