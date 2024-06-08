package com.adrninistrator.jacg.handler.common.enums;

/**
 * @author adrninistrator
 * @date 2022/11/18
 * @description: 字段关联关联的标志枚举
 */
public enum FieldRelationshipFlagsEnum {
    FRF_SET_MYBATIS_INSERT_ENTITY(1, "set方法对应MyBatis的insert操作，使用MyBatis Entity"),
    FRF_GET_MYBATIS_SELECT_ENTITY(1 << 1, "get方法对应MyBatis的select操作，返回MyBatis Entity"),
    FRF_SET_MYBATIS_UPDATE_SET_ENTITY(1 << 2, "set方法对应MyBatis的update set操作，使用MyBatis Entity"),
    FRF_SET_MYBATIS_UPDATE_WHERE_ENTITY(1 << 3, "set方法对应MyBatis的update where操作，使用MyBatis Entity"),
    FRF_SET_MYBATIS_MAPPER_ARG_OPERATE(1 << 4, "MyBatis的Mapper方法参数对应的操作"),
    FRF_GET_MYBATIS_SELECT_OBJECT(1 << 5, "get方法对应MyBatis的select操作，返回自定义对象"),
    FRF_SET_METHOD_CALL_PASSED(1 << 6, "set方法通过方法调用传递的字段赋值"),
    ;

    // 需要定义为1，以及2的幂
    private final int flag;

    private final String desc;

    FieldRelationshipFlagsEnum(int flag, String desc) {
        this.flag = flag;
        this.desc = desc;
    }

    /**
     * 判断方法调用标志是否包含指定的标志位
     *
     * @param fieldRelationshipFlag
     * @return
     */
    public boolean checkFlag(int fieldRelationshipFlag) {
        return (fieldRelationshipFlag & flag) != 0;
    }

    /**
     * 设置方法调用标志指定的标志位
     *
     * @param fieldRelationshipFlag
     * @return
     */
    public int setFlag(int fieldRelationshipFlag) {
        return fieldRelationshipFlag | flag;
    }

    public int getFlag() {
        return flag;
    }

    public String getDesc() {
        return desc;
    }

    @Override
    public String toString() {
        return flag + "-" + desc;
    }
}
