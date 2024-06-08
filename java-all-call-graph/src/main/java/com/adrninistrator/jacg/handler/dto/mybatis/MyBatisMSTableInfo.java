package com.adrninistrator.jacg.handler.dto.mybatis;

import com.adrninistrator.jacg.handler.dto.businessdata.MethodBehaviorByMyBatisMySqlTable;
import com.adrninistrator.mybatismysqltableparser.common.enums.MySqlStatementEnum;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/3/16
 * @description: MyBatis中操作的数据库表信息（支持MySQL）
 */
public class MyBatisMSTableInfo {
    // select语句的表名列表
    @JsonProperty("select")
    private List<String> selectTableList;

    // select for update语句的表名列表
    @JsonProperty("select4update")
    private List<String> select4UpdateTableList;

    // insert语句的表名列表
    @JsonProperty("insert")
    private List<String> insertTableList;

    // insert ignore into语句的表名列表
    @JsonProperty("insert_ignore")
    private List<String> insertIgnoreTableList;

    // insert into on duplicate key update语句的表名列表
    @JsonProperty("insert_update")
    private List<String> insertOrUpdateTableList;

    // replace into语句的表名列表
    @JsonProperty("replace")
    private List<String> replaceTableList;

    // update语句的表名列表
    @JsonProperty("update")
    private List<String> updateTableList;

    // delete语句的表名列表
    @JsonProperty("delete")
    private List<String> deleteTableList;

    // alter table语句的表名列表
    @JsonProperty("alter")
    private List<String> alterTableList;

    // truncate table语句的表名列表
    @JsonProperty("truncate")
    private List<String> truncateTableList;

    // create table语句的表名列表
    @JsonProperty("create")
    private List<String> createTableList;

    // drop table语句的表名列表
    @JsonProperty("drop")
    private List<String> dropTableList;

    /**
     * 获取当前主要处理的SQL语句类型，及相关的数据库表
     *
     * @return
     */
    @JsonIgnore
    public MyBatisMSStatementAndTables getMyBatisMySqlStatementAndTables() {
        if (select4UpdateTableList != null) {
            return new MyBatisMSStatementAndTables(MySqlStatementEnum.DSSE_SELECT_4_UPDATE, select4UpdateTableList);
        }
        if (insertTableList != null) {
            return new MyBatisMSStatementAndTables(MySqlStatementEnum.DSSE_INSERT, insertTableList);
        }
        if (insertIgnoreTableList != null) {
            return new MyBatisMSStatementAndTables(MySqlStatementEnum.DSSE_INSERT_IGNORE, insertIgnoreTableList);
        }
        if (insertOrUpdateTableList != null) {
            return new MyBatisMSStatementAndTables(MySqlStatementEnum.DSSE_INSERT_OR_UPDATE, insertOrUpdateTableList);
        }
        if (replaceTableList != null) {
            return new MyBatisMSStatementAndTables(MySqlStatementEnum.DSSE_REPLACE, replaceTableList);
        }
        if (updateTableList != null) {
            return new MyBatisMSStatementAndTables(MySqlStatementEnum.DSSE_UPDATE, updateTableList);
        }
        if (deleteTableList != null) {
            return new MyBatisMSStatementAndTables(MySqlStatementEnum.DSSE_DELETE, deleteTableList);
        }
        if (alterTableList != null) {
            return new MyBatisMSStatementAndTables(MySqlStatementEnum.DSSE_ALTER, alterTableList);
        }
        if (truncateTableList != null) {
            return new MyBatisMSStatementAndTables(MySqlStatementEnum.DSSE_TRUNCATE, truncateTableList);
        }
        if (createTableList != null) {
            return new MyBatisMSStatementAndTables(MySqlStatementEnum.DSSE_CREATE, createTableList);
        }
        if (dropTableList != null) {
            return new MyBatisMSStatementAndTables(MySqlStatementEnum.DSSE_DROP, dropTableList);
        }
        return new MyBatisMSStatementAndTables(MySqlStatementEnum.DSSE_SELECT, selectTableList);
    }

    /**
     * 获取MyBatis对应的操作及数据库表名对象列表
     *
     * @return
     */
    @JsonIgnore
    public List<MethodBehaviorByMyBatisMySqlTable> getMethodBehaviorByMyBatisMySqlTableList() {
        if (select4UpdateTableList != null) {
            return getMethodBehaviorByMyBatisMySqlTableList(MySqlStatementEnum.DSSE_SELECT_4_UPDATE, select4UpdateTableList);
        }
        if (insertTableList != null) {
            return getMethodBehaviorByMyBatisMySqlTableList(MySqlStatementEnum.DSSE_INSERT, insertTableList);
        }
        if (insertIgnoreTableList != null) {
            return getMethodBehaviorByMyBatisMySqlTableList(MySqlStatementEnum.DSSE_INSERT_IGNORE, insertIgnoreTableList);
        }
        if (insertOrUpdateTableList != null) {
            return getMethodBehaviorByMyBatisMySqlTableList(MySqlStatementEnum.DSSE_INSERT_OR_UPDATE, insertOrUpdateTableList);
        }
        if (replaceTableList != null) {
            return getMethodBehaviorByMyBatisMySqlTableList(MySqlStatementEnum.DSSE_REPLACE, replaceTableList);
        }
        if (updateTableList != null) {
            return getMethodBehaviorByMyBatisMySqlTableList(MySqlStatementEnum.DSSE_UPDATE, updateTableList);
        }
        if (deleteTableList != null) {
            return getMethodBehaviorByMyBatisMySqlTableList(MySqlStatementEnum.DSSE_DELETE, deleteTableList);
        }
        if (alterTableList != null) {
            return getMethodBehaviorByMyBatisMySqlTableList(MySqlStatementEnum.DSSE_ALTER, alterTableList);
        }
        if (truncateTableList != null) {
            return getMethodBehaviorByMyBatisMySqlTableList(MySqlStatementEnum.DSSE_TRUNCATE, truncateTableList);
        }
        if (createTableList != null) {
            return getMethodBehaviorByMyBatisMySqlTableList(MySqlStatementEnum.DSSE_CREATE, createTableList);
        }
        if (dropTableList != null) {
            return getMethodBehaviorByMyBatisMySqlTableList(MySqlStatementEnum.DSSE_DROP, dropTableList);
        }
        return getMethodBehaviorByMyBatisMySqlTableList(MySqlStatementEnum.DSSE_SELECT, selectTableList);
    }

    private List<MethodBehaviorByMyBatisMySqlTable> getMethodBehaviorByMyBatisMySqlTableList(MySqlStatementEnum mySqlStatementEnum, List<String> tableList) {
        List<MethodBehaviorByMyBatisMySqlTable> methodBehaviorByMyBatisMySqlTableList = new ArrayList<>();
        for (String tableName : tableList) {
            methodBehaviorByMyBatisMySqlTableList.add(new MethodBehaviorByMyBatisMySqlTable(mySqlStatementEnum.getInitials(), tableName));
        }
        if (MySqlStatementEnum.DSSE_SELECT != mySqlStatementEnum && selectTableList != null) {
            // 处理select语句
            for (String tableName : selectTableList) {
                methodBehaviorByMyBatisMySqlTableList.add(new MethodBehaviorByMyBatisMySqlTable(MySqlStatementEnum.DSSE_SELECT.getInitials(), tableName));
            }
        }
        return methodBehaviorByMyBatisMySqlTableList;
    }

    //
    public List<String> getSelectTableList() {
        return selectTableList;
    }

    public void setSelectTableList(List<String> selectTableList) {
        this.selectTableList = selectTableList;
    }

    public List<String> getSelect4UpdateTableList() {
        return select4UpdateTableList;
    }

    public void setSelect4UpdateTableList(List<String> select4UpdateTableList) {
        this.select4UpdateTableList = select4UpdateTableList;
    }

    public List<String> getInsertTableList() {
        return insertTableList;
    }

    public void setInsertTableList(List<String> insertTableList) {
        this.insertTableList = insertTableList;
    }

    public List<String> getInsertIgnoreTableList() {
        return insertIgnoreTableList;
    }

    public void setInsertIgnoreTableList(List<String> insertIgnoreTableList) {
        this.insertIgnoreTableList = insertIgnoreTableList;
    }

    public List<String> getInsertOrUpdateTableList() {
        return insertOrUpdateTableList;
    }

    public void setInsertOrUpdateTableList(List<String> insertOrUpdateTableList) {
        this.insertOrUpdateTableList = insertOrUpdateTableList;
    }

    public List<String> getReplaceTableList() {
        return replaceTableList;
    }

    public void setReplaceTableList(List<String> replaceTableList) {
        this.replaceTableList = replaceTableList;
    }

    public List<String> getUpdateTableList() {
        return updateTableList;
    }

    public void setUpdateTableList(List<String> updateTableList) {
        this.updateTableList = updateTableList;
    }

    public List<String> getDeleteTableList() {
        return deleteTableList;
    }

    public void setDeleteTableList(List<String> deleteTableList) {
        this.deleteTableList = deleteTableList;
    }

    public List<String> getAlterTableList() {
        return alterTableList;
    }

    public void setAlterTableList(List<String> alterTableList) {
        this.alterTableList = alterTableList;
    }

    public List<String> getTruncateTableList() {
        return truncateTableList;
    }

    public void setTruncateTableList(List<String> truncateTableList) {
        this.truncateTableList = truncateTableList;
    }

    public List<String> getCreateTableList() {
        return createTableList;
    }

    public void setCreateTableList(List<String> createTableList) {
        this.createTableList = createTableList;
    }

    public List<String> getDropTableList() {
        return dropTableList;
    }

    public void setDropTableList(List<String> dropTableList) {
        this.dropTableList = dropTableList;
    }
}
