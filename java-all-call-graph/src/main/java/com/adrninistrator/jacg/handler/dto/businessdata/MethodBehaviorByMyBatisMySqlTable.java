package com.adrninistrator.jacg.handler.dto.businessdata;

/**
 * @author adrninistrator
 * @date 2023/9/10
 * @description: MyBatis对应的操作及数据库表名
 */
public class MethodBehaviorByMyBatisMySqlTable extends MethodBehaviorByBusinessData {

    public static final String TYPE = "MyBatisMySqlTable";

    // 数据库表操作类型
    private final String mySqlStatementType;

    // 数据库表名
    private final String tableName;

    public MethodBehaviorByMyBatisMySqlTable(String mySqlStatementType, String tableName) {
        super(TYPE);
        this.mySqlStatementType = mySqlStatementType;
        this.tableName = tableName;
    }

    public String getMySqlStatementType() {
        return mySqlStatementType;
    }

    public String getTableName() {
        return tableName;
    }
}
