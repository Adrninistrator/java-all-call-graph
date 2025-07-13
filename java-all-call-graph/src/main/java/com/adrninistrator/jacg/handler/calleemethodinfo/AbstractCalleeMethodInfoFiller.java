package com.adrninistrator.jacg.handler.calleemethodinfo;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.calleemethodinfo.BaseCalleeMethodInfo;
import com.adrninistrator.jacg.handler.base.BaseHandler;

/**
 * @author adrninistrator
 * @date 2025/6/12
 * @description: 被调用方法信息填充接口
 */
public abstract class AbstractCalleeMethodInfoFiller extends BaseHandler {

    public AbstractCalleeMethodInfoFiller(ConfigureWrapper configureWrapper, String tableSuffix) {
        super(configureWrapper, tableSuffix);
    }

    public AbstractCalleeMethodInfoFiller(ConfigureWrapper configureWrapper) {
        super(configureWrapper, JACGConstants.TABLE_SUFFIX_NEW);
    }

    public AbstractCalleeMethodInfoFiller(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
    }

    /**
     * 查询指定被调用方法信息
     *
     * @param calleeFullMethod
     * @return
     */
    public abstract BaseCalleeMethodInfo query(String calleeFullMethod);
}
