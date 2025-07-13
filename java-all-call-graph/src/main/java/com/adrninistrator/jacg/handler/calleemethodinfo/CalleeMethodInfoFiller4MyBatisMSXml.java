package com.adrninistrator.jacg.handler.calleemethodinfo;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.calleemethodinfo.BaseCalleeMethodInfo;
import com.adrninistrator.jacg.dto.calleemethodinfo.CalleeMethodInfo4MyBatisMSXml;
import com.adrninistrator.jacg.dto.method.MethodDetailNoReturnType;
import com.adrninistrator.jacg.handler.mybatis.MybatisMsFormatedSqlHandler;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import org.apache.commons.lang3.StringUtils;

/**
 * @author adrninistrator
 * @date 2025/6/12
 * @description: 被调用方法信息填充类，处理被调用方法对应的MyBatis XML（支持MySQL）
 */
public class CalleeMethodInfoFiller4MyBatisMSXml extends AbstractCalleeMethodInfoFiller {

    private final MybatisMsFormatedSqlHandler mybatisMsFormatedSqlHandler;

    public CalleeMethodInfoFiller4MyBatisMSXml(ConfigureWrapper configureWrapper, String tableSuffix) {
        super(configureWrapper, tableSuffix);
        mybatisMsFormatedSqlHandler = new MybatisMsFormatedSqlHandler(dbOperWrapper);
    }

    public CalleeMethodInfoFiller4MyBatisMSXml(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
        mybatisMsFormatedSqlHandler = new MybatisMsFormatedSqlHandler(dbOperWrapper);
    }

    public CalleeMethodInfoFiller4MyBatisMSXml(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
        mybatisMsFormatedSqlHandler = new MybatisMsFormatedSqlHandler(dbOperWrapper);
    }

    @Override
    public BaseCalleeMethodInfo query(String calleeFullMethod) {
        MethodDetailNoReturnType methodDetailNoReturnType = JACGClassMethodUtil.genMethodDetailNoReturnType(calleeFullMethod);
        String xmlFilePath = mybatisMsFormatedSqlHandler.queryMapperXmlFilePath(methodDetailNoReturnType.getClassName(), methodDetailNoReturnType.getMethodName(),
                JACGConstants.TABLE_SUFFIX_NEW);
        if (StringUtils.isBlank(xmlFilePath)) {
            return null;
        }
        CalleeMethodInfo4MyBatisMSXml calleeMethodInfo4MyBatisMSXml = new CalleeMethodInfo4MyBatisMSXml();
        calleeMethodInfo4MyBatisMSXml.setXmlFilePath(xmlFilePath);
        return calleeMethodInfo4MyBatisMSXml;
    }
}
