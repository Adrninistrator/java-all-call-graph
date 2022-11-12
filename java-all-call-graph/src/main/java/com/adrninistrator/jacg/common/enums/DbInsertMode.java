package com.adrninistrator.jacg.common.enums;

/**
 * @author adrninistrator
 * @date 2022/8/21
 * @description: 插入数据库的模式
 */
public enum DbInsertMode {
    DIME_INSERT("insert into "),
    DIME_INSERT_IGNORE("insert ignore into "),
    DIME_REPLACE("replace into "),
    ;

    // 以上mode会用于sql语句拼接，需要以空格结尾
    private final String mode;

    DbInsertMode(String mode) {
        this.mode = mode;
    }

    public String getMode() {
        return mode;
    }
}
