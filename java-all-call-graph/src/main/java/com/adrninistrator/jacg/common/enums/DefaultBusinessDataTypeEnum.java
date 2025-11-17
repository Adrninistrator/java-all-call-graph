package com.adrninistrator.jacg.common.enums;

import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/3/21
 * @description: 默认的业务功能数据类型枚举
 */
public enum DefaultBusinessDataTypeEnum {
    BDTE_METHOD_CALL_INFO("method_call_info", "方法调用信息，被调用对象、参数的类型、值", true, true),
    BDTE_METHOD_ARG_GENERICS_TYPE("method_arg_generics_type", "方法参数泛型类型", true, true),
    BDTE_METHOD_RETURN_GENERICS_TYPE("method_return_generics_type", "方法返回泛型类型", true, true),
    BDTE_MYBATIS_MYSQL_TABLE("mybatis_mysql_table", "MyBatis的XML文件中对应的数据库表名（支持MySQL数据库）", false, true),
    BDTE_MYBATIS_MYSQL_WRITE_TABLE("mybatis_mysql_write_table", "MyBatis的XML文件中对应的写数据库表名（支持MySQL数据库）", false, true),
    BDTE_ILLEGAL("!illegal!", "非法值", false, false),
    ;

    // 类型
    private final String type;
    // 描述
    private final String desc;
    // 支持在生成向上的方法完整调用链时使用
    private final boolean supportEe;
    // 支持在生成向下的方法完整调用链时使用
    private final boolean supportEr;

    DefaultBusinessDataTypeEnum(String type, String desc, boolean supportEe, boolean supportEr) {
        this.type = type;
        this.desc = desc;
        this.supportEe = supportEe;
        this.supportEr = supportEr;
    }

    public static DefaultBusinessDataTypeEnum getFromType(String type) {
        for (DefaultBusinessDataTypeEnum businessDataTypeEnum : DefaultBusinessDataTypeEnum.values()) {
            if (businessDataTypeEnum.getType().equals(type)) {
                return businessDataTypeEnum;
            }
        }
        return DefaultBusinessDataTypeEnum.BDTE_ILLEGAL;
    }

    public static String getSupportTypeStr(boolean supportEe) {
        List<String> list = new ArrayList<>();
        for (DefaultBusinessDataTypeEnum defaultBusinessDataTypeEnum : DefaultBusinessDataTypeEnum.values()) {
            if (supportEe) {
                if (defaultBusinessDataTypeEnum.isSupportEe()) {
                    list.add(defaultBusinessDataTypeEnum.getType());
                }
            } else if (defaultBusinessDataTypeEnum.isSupportEr()) {
                list.add(defaultBusinessDataTypeEnum.getType());
            }
        }
        return StringUtils.join(list, " ");
    }

    public String getType() {
        return type;
    }

    public String getDesc() {
        return desc;
    }

    public boolean isSupportEe() {
        return supportEe;
    }

    public boolean isSupportEr() {
        return supportEr;
    }
}
