package com.adrninistrator.jacg.handler.dto.mybatis;

import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/3/16
 * @description: MyBatis中操作的数据库表信息（支持MySQL）
 */
public class MyBatisMySqlTableInfo {
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
