package com.adrninistrator.jacg.druidfilter;

import com.alibaba.druid.filter.FilterChain;
import com.alibaba.druid.filter.FilterEventAdapter;
import com.alibaba.druid.proxy.jdbc.ConnectionProxy;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.sql.SQLException;
import java.util.Properties;

/**
 * @author adrninistrator
 * @date 2023/8/5
 * @description: druid数据源监控过滤器
 */
public class DruidMonitorFilter extends FilterEventAdapter {
    private static final Logger logger = LoggerFactory.getLogger(DruidMonitorFilter.class);

    private final boolean useH2Db;

    public DruidMonitorFilter(boolean useH2Db) {
        this.useH2Db = useH2Db;
    }

    @Override
    public ConnectionProxy connection_connect(FilterChain chain, Properties info) throws SQLException {
        try {
            return chain.connection_connect(info);
        } catch (Exception e) {
            logger.error("\n连接数据库出现异常 {} ", (useH2Db ? "请先关闭H2数据库工具打开的H2数据库文件后重试" : ""), e);
            throw e;
        }
    }
}
