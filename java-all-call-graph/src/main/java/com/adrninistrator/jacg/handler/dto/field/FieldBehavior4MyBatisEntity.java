package com.adrninistrator.jacg.handler.dto.field;

/**
 * @author adrninistrator
 * @date 2023/7/30
 * @description: 类名、字段名及对应的get/set方法名，MyBatis的Entity的信息
 */
public class FieldBehavior4MyBatisEntity extends FieldBehavior {

    public static final String TYPE = "MyBatisEntity";

    // 数据库表名
    private String tableName;

    // 数据库字段名
    private String columnName;

    // 数据库字段的操作类型
    private String dbOperate;

    // 与Java代码字段关联方式描述
    private String desc;

    public FieldBehavior4MyBatisEntity(FieldBehavior fieldBehavior, String tableName, String columnName, String dbOperate, String desc) {
        super(TYPE, fieldBehavior.className, fieldBehavior.fieldName, fieldBehavior.fieldType, fieldBehavior.getMethodName, fieldBehavior.setMethodName, fieldBehavior.getOrSet,
                fieldBehavior.relationShipFlags, fieldBehavior.nestedField);
        this.tableName = tableName;
        this.columnName = columnName;
        this.dbOperate = dbOperate;
        this.desc = desc;
    }

    public String getTableName() {
        return tableName;
    }

    public void setTableName(String tableName) {
        this.tableName = tableName;
    }

    public String getColumnName() {
        return columnName;
    }

    public void setColumnName(String columnName) {
        this.columnName = columnName;
    }

    public String getDbOperate() {
        return dbOperate;
    }

    public void setDbOperate(String dbOperate) {
        this.dbOperate = dbOperate;
    }

    public String getDesc() {
        return desc;
    }

    public void setDesc(String desc) {
        this.desc = desc;
    }

    @Override
    public String toString() {
        return "FieldBehavior4MyBatisEntity{" +
                "type='" + type + '\'' +
                ", className='" + className + '\'' +
                ", fieldName='" + fieldName + '\'' +
                ", fieldType='" + fieldType + '\'' +
                ", getMethodName='" + getMethodName + '\'' +
                ", setMethodName='" + setMethodName + '\'' +
                ", getOrSet=" + getOrSet +
                ", relationShipFlags=" + relationShipFlags +
                ", nestedField=" + nestedField +
                ", tableName='" + tableName + '\'' +
                ", columnName='" + columnName + '\'' +
                ", dbOperate='" + dbOperate + '\'' +
                ", desc='" + desc + '\'' +
                '}';
    }
}
