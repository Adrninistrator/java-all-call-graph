package com.adrninistrator.jacg.handler.dto.mybatis.mapper;

/**
 * @author adrninistrator
 * @date 2023/11/6
 * @description: MyBatis Mapper方法参数信息抽象父类
 */
public abstract class AbstractMyBatisMapperArg {

    // 参数类型
    private String argType;

    // 参数在sql语句中的名称
    private String argNameInSql;

    public String getArgType() {
        return argType;
    }

    public void setArgType(String argType) {
        this.argType = argType;
    }

    public String getArgNameInSql() {
        return argNameInSql;
    }

    public void setArgNameInSql(String argNameInSql) {
        this.argNameInSql = argNameInSql;
    }

    @Override
    public String toString() {
        return "AbstractMyBatisMapperArg{" +
                "argType='" + argType + '\'' +
                ", argNameInSql='" + argNameInSql + '\'' +
                '}';
    }
}
