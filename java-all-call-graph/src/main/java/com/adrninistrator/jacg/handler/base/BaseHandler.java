package com.adrninistrator.jacg.handler.base;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dboper.DbOperator;
import com.adrninistrator.javacg.exceptions.JavaCGRuntimeException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.Closeable;

/**
 * @author adrninistrator
 * @date 2023/1/5
 * @description: 处理类的基类，实现了Closeable接口，支持通过try-with-resource方法确保执行完毕时数据库被关闭
 */
public abstract class BaseHandler implements Closeable {
    private static final Logger logger = LoggerFactory.getLogger(BaseHandler.class);

    protected final DbOperator dbOperator;

    protected final DbOperWrapper dbOperWrapper;

    // 记录是否需要关闭数据库操作对象
    private boolean needCloseDb = false;

    /**
     * 生成可以直接使用的对象时调用该构造函数
     * 调用该构造函数时，结束前[需要]关闭数据库操作对象
     *
     * @param configureWrapper 配置包装类对象，不允许为null，可以为new出来的ConfigureWrapper对象
     */
    public BaseHandler(ConfigureWrapper configureWrapper) {
        if (configureWrapper == null) {
            throw new JavaCGRuntimeException("传入配置不允许为null");
        }

        // 完成需要使用的基础配置的初始化
        dbOperWrapper = DbOperWrapper.genInstance(configureWrapper, this.getClass().getSimpleName());
        dbOperator = dbOperWrapper.getDbOperator();

        logger.warn("调用该构造函数时，结束前[需要]关闭数据库操作对象");
        needCloseDb = true;
    }

    /**
     * 调用该构造函数时，结束前[不需要]关闭数据库操作对象
     *
     * @param dbOperWrapper
     */
    public BaseHandler(DbOperWrapper dbOperWrapper) {
        if (dbOperWrapper == null || dbOperWrapper.getDbOperator() == null) {
            throw new JavaCGRuntimeException("传入参数不允许为空");
        }

        this.dbOperator = dbOperWrapper.getDbOperator();
        this.dbOperWrapper = dbOperWrapper;
    }

    @Override
    public void close() {
        if (needCloseDb && dbOperator != null) {
            // 关闭数据源
            dbOperator.closeDs();
        }
    }
}
