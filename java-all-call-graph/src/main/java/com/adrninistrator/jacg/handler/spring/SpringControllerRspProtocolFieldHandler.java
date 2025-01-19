package com.adrninistrator.jacg.handler.spring;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.handler.dto.methodreturn.MethodReturnTypeWithGenerics;
import com.adrninistrator.jacg.handler.dto.spring.SpringControllerInfo;
import com.adrninistrator.jacg.handler.dto.spring.SpringControllerReturnTypeWithGenerics;
import com.adrninistrator.jacg.handler.protocol.QueryRspProtocolFieldHandler;
import com.adrninistrator.javacg2.util.JavaCG2Util;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2024/12/3
 * @description: 处理Spring Controller返回协议中的字段信息的处理类
 */
public class SpringControllerRspProtocolFieldHandler extends BaseHandler {

    private final SpringHandler springHandler;
    private final QueryRspProtocolFieldHandler queryRspProtocolFieldHandler;

    public SpringControllerRspProtocolFieldHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
        springHandler = new SpringHandler(dbOperWrapper);
        queryRspProtocolFieldHandler = new QueryRspProtocolFieldHandler(dbOperWrapper);
    }

    public SpringControllerRspProtocolFieldHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
        springHandler = new SpringHandler(dbOperWrapper);
        queryRspProtocolFieldHandler = new QueryRspProtocolFieldHandler(dbOperWrapper);
    }

    /**
     * 查询所有Spring Controller方法的返回类型，包含泛型类型
     *
     * @return
     */
    public List<SpringControllerReturnTypeWithGenerics> queryAllReturnTypeWithGenerics() {
        List<SpringControllerInfo> springControllerInfoList = springHandler.queryAllControllerInfo();
        if (JavaCG2Util.isCollectionEmpty(springControllerInfoList)) {
            return Collections.emptyList();
        }
        List<SpringControllerReturnTypeWithGenerics> list = new ArrayList<>();
        for (SpringControllerInfo springControllerInfo : springControllerInfoList) {
            SpringControllerReturnTypeWithGenerics springControllerReturnTypeWithGenerics = new SpringControllerReturnTypeWithGenerics();
            list.add(springControllerReturnTypeWithGenerics);
            springControllerReturnTypeWithGenerics.setSpringControllerInfo(springControllerInfo);
            // 查询Spring Controller方法的返回类型，包含泛型类型
            MethodReturnTypeWithGenerics methodReturnTypeWithGenerics = queryRspProtocolFieldHandler.queryMethodReturnTypeWithGenerics(springControllerInfo.getFullMethod());
            springControllerReturnTypeWithGenerics.setMethodReturnTypeWithGenerics(methodReturnTypeWithGenerics);
        }
        return list;
    }
}
