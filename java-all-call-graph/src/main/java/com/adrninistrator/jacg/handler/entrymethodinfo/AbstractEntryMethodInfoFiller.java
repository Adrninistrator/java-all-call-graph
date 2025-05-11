package com.adrninistrator.jacg.handler.entrymethodinfo;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.entrymethodinfo.BaseEntryMethodInfo;
import com.adrninistrator.jacg.handler.base.BaseHandler;

/**
 * @author adrninistrator
 * @date 2024/3/24
 * @description: 入口方法信息填充接口
 */
public abstract class AbstractEntryMethodInfoFiller extends BaseHandler {
    public AbstractEntryMethodInfoFiller(ConfigureWrapper configureWrapper, String tableSuffix) {
        super(configureWrapper, tableSuffix);
    }

    public AbstractEntryMethodInfoFiller(ConfigureWrapper configureWrapper) {
        super(configureWrapper, JACGConstants.TABLE_SUFFIX_NEW);
    }

    public AbstractEntryMethodInfoFiller(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
    }

    /**
     * 查询指定的入口方法与返回类型对应的信息
     *
     * @param entryMethod
     * @param entryMethodReturnType
     * @return
     */
    public abstract BaseEntryMethodInfo query(String entryMethod, String entryMethodReturnType);
}
