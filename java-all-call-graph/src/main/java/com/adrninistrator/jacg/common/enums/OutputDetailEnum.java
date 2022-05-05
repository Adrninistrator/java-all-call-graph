package com.adrninistrator.jacg.common.enums;

/**
 * @author adrninistrator
 * @date 2022/4/20
 * @description:
 */
public enum OutputDetailEnum {
    ODE_1("1", "输出结果展示：完整类名+方法名+方法参数"),
    ODE_2("2", "输出结果展示：完整类名+方法名"),
    ODE_3("3", "输出结果展示：简单类名（对于同名类展示完整类名）+方法名"),
    ODE_ILLEGAL("ILLEGAL", "非法"),
    ;

    private String detail;
    private String desc;

    OutputDetailEnum(String detail, String desc) {
        this.detail = detail;
        this.desc = desc;
    }

    public String getDetail() {
        return detail;
    }

    public String getDesc() {
        return desc;
    }

    public static OutputDetailEnum getFromDetail(String detail) {
        for (OutputDetailEnum outputDetailEnum : OutputDetailEnum.values()) {
            if (outputDetailEnum.getDetail().equals(detail)) {
                return outputDetailEnum;
            }
        }
        return OutputDetailEnum.ODE_ILLEGAL;
    }

    @Override
    public String toString() {
        return detail;
    }
}
