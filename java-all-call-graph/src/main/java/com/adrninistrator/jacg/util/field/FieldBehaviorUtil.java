package com.adrninistrator.jacg.util.field;

import com.adrninistrator.jacg.handler.dto.field.FieldBehavior;

import java.util.Collections;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/9/25
 * @description: 字段行为工具类
 */
public class FieldBehaviorUtil {

    // 代表不存在可填充字段行为的值
    public static final FieldBehavior NONE_FIELD_BEHAVIOR = new FieldBehavior(FieldBehavior.TYPE, null, null, null, null, false, 0);

    // 代表不存在可填充字段行为的值列表
    public static final List<FieldBehavior> NONE_FIELD_BEHAVIOR_LIST = Collections.singletonList(NONE_FIELD_BEHAVIOR);

    /**
     * 判断指定的字段行为是否为代表不存在可填充字段行为
     *
     * @param fieldBehavior 指定的字段行为
     * @return true: 代表不存在可填充字段行为 false: 不是代表不存在可填充字段行为
     */
    public static boolean checkNoneFieldBehavior(FieldBehavior fieldBehavior) {
        return fieldBehavior != null && FieldBehavior.TYPE.equals(fieldBehavior.getType());
    }

    /**
     * 判断指定的字段行为列表是否为代表不存在可填充字段行为的列表
     *
     * @param fieldBehaviorList 指定的字段行为列表
     * @return true: 代表不存在可填充字段行为的列表 false: 不是代表不存在可填充字段行为的列表
     */
    public static boolean checkNoneFieldBehaviorList(List<FieldBehavior> fieldBehaviorList) {
        if (fieldBehaviorList == null) {
            return false;
        }
        return fieldBehaviorList.size() == 1 && checkNoneFieldBehavior(fieldBehaviorList.get(0));
    }

    private FieldBehaviorUtil() {
        throw new IllegalStateException("illegal");
    }
}
