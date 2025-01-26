package test.callgraph.enums;

/**
 * @author adrninistrator
 * @date 2021/9/9
 * @description: 数据库语句类型枚举
 */
public enum DbStatementEnum {

    DSE_SELECT("select", "查询"),
    DSE_INSERT("insert", "插入"),
    DSE_REPLACE("replace", "替换"),
    DSE_UPDATE("update", "更新"),
    DSE_DELETE("delete", "删除"),
    DSE_TEST("test", "测试\ta\r\nb"),
    DSE_ILLEGAL("-", "-");

    private final String statement;
    private final String desc;

    DbStatementEnum(String statement, String desc) {
        this.statement = statement;
        this.desc = desc;
    }

    public static DbStatementEnum getFromStatement(String statement) {
        for (DbStatementEnum dbStatementEnum : DbStatementEnum.values()) {
            if (dbStatementEnum.getStatement().equals(statement)) {
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

    public String getDescFromStatement(String statement) {
        return DbStatementEnum.getFromStatement(statement).desc;
    }
}
