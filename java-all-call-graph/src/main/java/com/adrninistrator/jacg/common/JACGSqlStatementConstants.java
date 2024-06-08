package com.adrninistrator.jacg.common;

import com.adrninistrator.mybatismysqltableparser.common.enums.MySqlStatementEnum;

/**
 * @author adrninistrator
 * @date 2023/11/4
 * @description:
 */
public class JACGSqlStatementConstants {

    public static final String SET = "set";
    public static final String WHERE = "where";

    public static final String INSERT = MySqlStatementEnum.DSSE_INSERT.getInitials();
    public static final String SELECT = MySqlStatementEnum.DSSE_SELECT.getInitials();
    public static final String UPDATE = MySqlStatementEnum.DSSE_UPDATE.getInitials();
    public static final String DELETE = MySqlStatementEnum.DSSE_DELETE.getInitials();

    public static final String UPDATE_SET = UPDATE + JACGConstants.FLAG_AT + SET;

    public static final String SELECT_WHERE = SELECT + JACGConstants.FLAG_AT + WHERE;
    public static final String UPDATE_WHERE = UPDATE + JACGConstants.FLAG_AT + WHERE;
    public static final String DELETE_WHERE = DELETE + JACGConstants.FLAG_AT + WHERE;

    private JACGSqlStatementConstants() {
        throw new IllegalStateException("illegal");
    }
}
