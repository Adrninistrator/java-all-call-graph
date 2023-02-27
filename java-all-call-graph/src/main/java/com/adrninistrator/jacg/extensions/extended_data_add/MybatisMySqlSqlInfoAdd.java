package com.adrninistrator.jacg.extensions.extended_data_add;

import com.adrninistrator.jacg.dto.method.MethodDetail;
import com.adrninistrator.jacg.dto.method_call.ObjArgsInfoInMethodCall;
import com.adrninistrator.jacg.extensions.code_parser.jar_entry_other_file.MyBatisMySqlSqlInfoCodeParser;
import com.adrninistrator.jacg.extensions.dto.extened_data.BaseExtendedData;
import com.adrninistrator.jacg.extensions.dto.mysql.JACGMySqlTableInfo;
import com.adrninistrator.jacg.util.JACGJsonUtil;
import com.adrninistrator.javacg.enums.CallTypeEnum;
import com.adrninistrator.mybatis_mysql_table_parser.dto.MyBatisSqlInfo;
import com.adrninistrator.mybatis_mysql_table_parser.dto.MySqlTableInfo;

/**
 * @author adrninistrator
 * @date 2023/1/2
 * @description: 将MyBatis的XML文件中对应的数据库表名添加为方法调用自定义数据（支持MySQL数据库）
 */
public class MybatisMySqlSqlInfoAdd implements ExtendedDataAddInterface {
    public static final String DATA_TYPE = "MYB_MYS";

    // 判断是否处理当前方法调用
    @Override
    public boolean checkNeedHandle(String callType, MethodDetail calleeMethodDetail) {
        // 仅处理INVOKEINTERFACE类型的方法调用，对应MyBatis Mapper的使用方式
        return CallTypeEnum.CTE_RAW_INVOKE_INTERFACE.getType().equals(callType);
    }

    // 生成方法调用自定义数据
    @Override
    public BaseExtendedData genBaseExtendedData(String callType, MethodDetail calleeMethodDetail, ObjArgsInfoInMethodCall objArgsInfoInMethodCall) {
        // 根据当前被调用的类名，尝试获取对应MyBatis的sql信息
        MyBatisSqlInfo myBatisSqlInfo = MyBatisMySqlSqlInfoCodeParser.getLastInstance().getMyBatisSqlInfo(calleeMethodDetail.getClassName());
        if (myBatisSqlInfo == null) {
            return null;
        }

        // 根据当前被调用的方法名，尝试获取对应MyBatis的表名信息
        MySqlTableInfo mySqlTableInfo = myBatisSqlInfo.getMySqlTableInfoMap().get(calleeMethodDetail.getMethodName());
        if (mySqlTableInfo == null) {
            return null;
        }

        // 对相关的数据库表名进行JSON序列化
        JACGMySqlTableInfo jacgMySqlTableInfo = new JACGMySqlTableInfo(mySqlTableInfo);

        String dataValue = JACGJsonUtil.getJsonStr(jacgMySqlTableInfo);
        return new BaseExtendedData(DATA_TYPE, dataValue);
    }
}
