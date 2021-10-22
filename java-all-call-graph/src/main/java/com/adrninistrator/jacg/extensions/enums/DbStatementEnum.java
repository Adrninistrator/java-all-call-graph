package com.adrninistrator.jacg.extensions.enums;

/**
 * @author adrninistrator
 * @date 2021/9/9
 * @description:
 */
public enum DbStatementEnum {

    DSE_SELECT("select", "查询"),
    DSE_INSERT("insert", "插入"),
    DSE_REPLACE("replace", "替换"),
    DSE_UPDATE("update", "更新"),
    DSE_DELETE("delete", "删除"),
    DSE_ILLEGAL("-", "-");

    private String statement;
    private String desc;

    DbStatementEnum(String statement, String desc) {
        this.statement = statement;
        this.desc = desc;
    }

    public static DbStatementEnum getFromStatement(String type) {
        for (DbStatementEnum dbStatementEnum : DbStatementEnum.values()) {
            if (dbStatementEnum.getStatement().equals(type)) {
                return dbStatementEnum;
            }
        }
        return DbStatementEnum.DSE_ILLEGAL;
    }

    public String getStatement() {
        return statement;
    }

    public String getDesc() {
        return desc;
    }
}
