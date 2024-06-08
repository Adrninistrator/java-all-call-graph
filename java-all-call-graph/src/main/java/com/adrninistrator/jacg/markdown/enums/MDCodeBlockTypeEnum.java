package com.adrninistrator.jacg.markdown.enums;

/**
 * @author adrninistrator
 * @date 2022/11/27
 * @description: markdown代码块类型枚举
 */
public enum MDCodeBlockTypeEnum {
    MDCBTE_SQL("sql"),
    MDCBTE_JAVA("java"),
    ;

    private final String type;

    MDCodeBlockTypeEnum(String type) {
        this.type = type;
    }

    public String getType() {
        return type;
    }
}
