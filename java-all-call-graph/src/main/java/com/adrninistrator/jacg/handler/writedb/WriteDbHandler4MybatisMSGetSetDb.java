package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.WriteDbHandlerWriteFileEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MybatisMSGetSetDb;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;

/**
 * @author adrninistrator
 * @date 2023/11/13
 * @description: 写入数据库，使用MyBatis时get/set方法所关联的数据库信息（使用MySQL）
 */
@JACGWriteDbHandler(
        readFile = false,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_MYBATIS_MS_GET_SET_DB,
        writeFileEnum = WriteDbHandlerWriteFileEnum.WDHWFE_MYBATIS_MS_GET_SET
)
public class WriteDbHandler4MybatisMSGetSetDb extends AbstractWriteDbHandler<WriteDbData4MybatisMSGetSetDb> {

    public WriteDbHandler4MybatisMSGetSetDb(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "记录id，从1开始",
                "通过get/set方法关联的字段关系id，从1开始",
                "对应get方法还是set方法",
                "get方法对应的方法调用ID，从1开始",
                "set方法对应的方法调用ID，从1开始",
                "数据库操作，包含sql语句，除select、insert、update、delete外，后面可能加上@set、@where",
                "数据库表名（MyBatis XML中可能使用函数，长度需要长一些）",
                "数据库字段名（MyBatis XML中可能使用函数，长度需要长一些）",
                "MyBatis字段与Java代码字段关联方式描述，参考 MyBatisColumnRelateDescEnum 枚举类"
        };
    }

    @Override
    public String chooseNotMainFileDesc() {
        return "使用MyBatis时get/set方法所关联的数据库信息（使用MySQL）";
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{"使用MySQL时，Java代码中的get/set方法通过MyBatis XML所关联的数据库信息"};
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4MybatisMSGetSetDb data) {
        return new Object[]{
                data.getRecordId(),
                data.getFldRelationshipId(),
                data.getGetOrSet(),
                data.getGetMethodCallId(),
                data.getSetMethodCallId(),
                data.getDbOperate(),
                data.getTableName(),
                data.getColumnName(),
                data.getColumnRelateDesc()
        };
    }
}
