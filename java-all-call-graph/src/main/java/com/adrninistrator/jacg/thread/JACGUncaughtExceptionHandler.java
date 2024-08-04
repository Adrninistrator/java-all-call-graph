package com.adrninistrator.jacg.thread;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author adrninistrator
 * @date 2024/3/23
 * @description:
 */
public class JACGUncaughtExceptionHandler implements Thread.UncaughtExceptionHandler {

    private static final Logger logger = LoggerFactory.getLogger(JACGUncaughtExceptionHandler.class);

    private int exceptionCount;

    @Override
    public void uncaughtException(Thread t, Throwable e) {
        if (e != null) {
            exceptionCount++;
            logger.error("线程池执行出现未捕获的异常 {} ", t.getName(), e);
        }
    }

    public int getExceptionCount() {
        return exceptionCount;
    }
}
