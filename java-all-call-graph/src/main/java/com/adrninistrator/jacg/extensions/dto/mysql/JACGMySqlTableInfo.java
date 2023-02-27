package com.adrninistrator.jacg.extensions.dto.mysql;

import com.adrninistrator.mybatis_mysql_table_parser.dto.MySqlTableInfo;
import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/1/2
 * @description: MySQL的sql语句中的表信息
 */
public class JACGMySqlTableInfo {
    // select语句的表名列表
    @JsonProperty("s")
    private List<String> selectTableList;

    // select for update语句的表名列表
    @JsonProperty("su")
    private List<String> select4UpdateTableList;

    // insert语句的表名列表
    @JsonProperty("i")
    private List<String> insertTableList;

    // insert ignore into语句的表名列表
    @JsonProperty("ii")
    private List<String> insertIgnoreTableList;

    // insert into on duplicate key update语句的表名列表
    @JsonProperty("iu")
    private List<String> insertOrUpdateTableList;

    // replace into语句的表名列表
    @JsonProperty("r")
    private List<String> replaceTableList;

    // update语句的表名列表
    @JsonProperty("u")
    private List<String> updateTableList;

    // delete语句的表名列表
    @JsonProperty("del")
    private List<String> deleteTableList;

    // alter table语句的表名列表
    @JsonProperty("a")
    private List<String> alterTableList;

    // truncate table语句的表名列表
    @JsonProperty("t")
    private List<String> truncateTableList;

    // create table语句的表名列表
    @JsonProperty("c")
    private List<String> createTableList;

    // drop table语句的表名列表
    @JsonProperty("drop")
    private List<String> dropTableList;

    public JACGMySqlTableInfo() {
    }

    public JACGMySqlTableInfo(MySqlTableInfo mySqlTableInfo) {
        if (!mySqlTableInfo.getSelectTableList().isEmpty()) {
            setSelectTableList(mySqlTableInfo.getSelectTableList());
        }
        if (!mySqlTableInfo.getSelect4UpdateTableList().isEmpty()) {
            setSelect4UpdateTableList(mySqlTableInfo.getSelect4UpdateTableList());
        }
        if (!mySqlTableInfo.getInsertTableList().isEmpty()) {
            setInsertTableList(mySqlTableInfo.getInsertTableList());
        }
        if (!mySqlTableInfo.getInsertIgnoreTableList().isEmpty()) {
            setInsertIgnoreTableList(mySqlTableInfo.getInsertIgnoreTableList());
        }
        if (!mySqlTableInfo.getInsertOrUpdateTableList().isEmpty()) {
            setInsertOrUpdateTableList(mySqlTableInfo.getInsertOrUpdateTableList());
        }
        if (!mySqlTableInfo.getReplaceTableList().isEmpty()) {
            setReplaceTableList(mySqlTableInfo.getReplaceTableList());
        }
        if (!mySqlTableInfo.getUpdateTableList().isEmpty()) {
            setUpdateTableList(mySqlTableInfo.getUpdateTableList());
        }
        if (!mySqlTableInfo.getDeleteTableList().isEmpty()) {
            setDeleteTableList(mySqlTableInfo.getDeleteTableList());
        }
        if (!mySqlTableInfo.getAlterTableList().isEmpty()) {
            setAlterTableList(mySqlTableInfo.getAlterTableList());
        }
        if (!mySqlTableInfo.getTruncateTableList().isEmpty()) {
            setTruncateTableList(mySqlTableInfo.getTruncateTableList());
        }
        if (!mySqlTableInfo.getCreateTableList().isEmpty()) {
            setCreateTableList(mySqlTableInfo.getCreateTableList());
        }
        if (!mySqlTableInfo.getDropTableList().isEmpty()) {
            setDropTableList(mySqlTableInfo.getDropTableList());
        }
    }

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
