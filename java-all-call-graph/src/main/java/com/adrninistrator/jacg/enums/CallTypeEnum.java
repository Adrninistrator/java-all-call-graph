package com.adrninistrator.jacg.enums;

/**
 * @author adrninistrator
 * @date 2021/8/2
 * @description:
 */

public enum CallTypeEnum {
    CTE_ITF("ITF", "INTERFACE"),
    CTE_RIR("RIR", "RUNNABLE_INIT_RUN"),
    CTE_CIC("CIC", "CALLABLE_INIT_CALL"),
    CTE_SCC("SCC", "SUPER_CALL_CHILD"),
    CTE_ILLEGAL("ILLEGAL", "ILLEGAL"),
    ;

    private String type;

    private String desc;

    CallTypeEnum(String type, String desc) {
        this.type = type;
        this.desc = desc;
    }

    public String getType() {
        return type;
    }

    public String getDesc() {
        return desc;
    }

    public static CallTypeEnum getFromType(String type) {
        for (CallTypeEnum callTypeEnum : CallTypeEnum.values()) {
            if (callTypeEnum.getType().equals(type)) {
                return callTypeEnum;
            }
        }
        return CallTypeEnum.CTE_ILLEGAL;
    }

    @Override
    public String toString() {
        return type + "-" + desc;
    }
}
