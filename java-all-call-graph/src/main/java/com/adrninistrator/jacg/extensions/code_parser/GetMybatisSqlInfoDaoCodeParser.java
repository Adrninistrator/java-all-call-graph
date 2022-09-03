package com.adrninistrator.jacg.extensions.code_parser;

/**
 * @author adrninistrator
 * @date 2021/11/2
 * @description: 从MyBatis的XML文件获取对应的数据库操作语句及被操作的数据库表名，处理MyBatis的Mapper接口包名为dao
 */
public class GetMybatisSqlInfoDaoCodeParser extends AbstractGetMybatisSqlInfoCodeParser {

    @Override
    protected boolean checkClassNameAndMethod(String calleeClassName, String calleeMethodName) {
        return calleeClassName.contains(".dao.");
    }
}
