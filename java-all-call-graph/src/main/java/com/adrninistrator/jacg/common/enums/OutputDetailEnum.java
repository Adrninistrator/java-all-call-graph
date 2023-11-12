package com.adrninistrator.jacg.common.enums;

/**
 * @author adrninistrator
 * @date 2022/4/20
 * @description: 输出结果展示详细程度枚举
 */
public enum OutputDetailEnum {
    ODE_0("0", "输出结果展示：0: 最详细 完整类名+方法名+方法参数+返回类型"),
    ODE_1("1", "输出结果展示：1: 详细 完整类名+方法名+方法参数"),
    ODE_2("2", "输出结果展示：2: 中等 完整类名+方法名"),
    ODE_3("3", "输出结果展示：3: 最简单 简单类名（对于同名类展示完整类名）+方法名"),
    ODE_ILLEGAL("ILLEGAL", "非法"),
    ;

    private final String detail;
    private final String desc;

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

    public static String getValidValues() {
        StringBuilder stringBuilder = new StringBuilder();
        for (OutputDetailEnum outputDetailEnum : OutputDetailEnum.values()) {
            if (OutputDetailEnum.ODE_ILLEGAL == outputDetailEnum) {
                continue;
            }
            if (stringBuilder.length() > 0) {
                stringBuilder.append(" ");
            }
            stringBuilder.append(outputDetailEnum.getDetail());
        }
        return stringBuilder.toString();
    }

    @Override
    public String toString() {
        return detail;
    }
}
