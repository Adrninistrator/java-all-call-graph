package com.adrninistrator.jacg.handler.spring;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dboper.DbOperator;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author adrninistrator
 * @date 2023/1/6
 * @description: Spring相关的查询处理类
 */
public class SpringHandler4Query extends BaseHandler {
    private static final Logger logger = LoggerFactory.getLogger(SpringHandler4Query.class);

    public SpringHandler4Query(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
    }

    /*
select full_method from jacg_spring_controller_test_rbc


select concat(class_name, ':', method_name, '()') from jacg_spring_task_test_rbc
     */
}