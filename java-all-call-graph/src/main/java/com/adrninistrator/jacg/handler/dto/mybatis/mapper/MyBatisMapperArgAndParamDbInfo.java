package com.adrninistrator.jacg.handler.dto.mybatis.mapper;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/11/8
 * @description: MyBatis Mapper方法参数信息，以及方法参数对应的数据库信息（支持MySQL）
 */
public class MyBatisMapperArgAndParamDbInfo {

    // 当前方法参数信息
    private AbstractMyBatisMapperArg myBatisMapperArg;

    // 当前参数在MyBatis sql语句中对应的数据库信息
    private List<MyBatisMSMapperParamDbInfo> myBatisMSMapperParamDbInfoList;

    public AbstractMyBatisMapperArg getMyBatisMapperArg() {
        return myBatisMapperArg;
    }

    public void setMyBatisMapperArg(AbstractMyBatisMapperArg myBatisMapperArg) {
        this.myBatisMapperArg = myBatisMapperArg;
    }

    public List<MyBatisMSMapperParamDbInfo> getMyBatisMSMapperParamDbInfoList() {
        return myBatisMSMapperParamDbInfoList;
    }

    public void setMyBatisMSMapperParamDbInfoList(List<MyBatisMSMapperParamDbInfo> myBatisMSMapperParamDbInfoList) {
        this.myBatisMSMapperParamDbInfoList = myBatisMSMapperParamDbInfoList;
    }
}
