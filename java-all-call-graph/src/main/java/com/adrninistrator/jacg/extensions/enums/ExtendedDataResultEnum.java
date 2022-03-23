package com.adrninistrator.jacg.extensions.enums;

/**
 * @author adrninistrator
 * @date 2021/11/1
 * @description: 自定义数据处理结果
 */
public enum ExtendedDataResultEnum {
    EDRE_SUCCESS("success", "处理成功"),
    EDRE_FAIL("fail", "处理失败"),
    EDRE_NONE("none", "不存在自定义数据"),
    ;

    private String result;

    private String desc;

    ExtendedDataResultEnum(String result, String desc) {
        this.result = result;
        this.desc = desc;
    }

    public String getResult() {
        return result;
    }

    public String getDesc() {
        return desc;
    }

    @Override
    public String toString() {
        return result + "-" + desc;
    }
}
