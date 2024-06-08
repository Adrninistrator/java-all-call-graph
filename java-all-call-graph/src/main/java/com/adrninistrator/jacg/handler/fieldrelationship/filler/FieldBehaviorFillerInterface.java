package com.adrninistrator.jacg.handler.fieldrelationship.filler;

import com.adrninistrator.jacg.handler.common.enums.FieldRelationshipIdTypeEnum;
import com.adrninistrator.jacg.handler.dto.field.FieldBehavior;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/7/30
 * @description: 为字段行为填充信息的接口
 */
public interface FieldBehaviorFillerInterface {

    /**
     * 初始化操作
     */
    default void init() {
    }

    /**
     * 填充字段信息
     *
     * @param fieldBehavior               填充前的字段信息父类对象
     * @param fieldRelationshipIdTypeEnum 代表当前使用的ID含义
     * @param id                          ID，含义由前一个参数决定
     * @return null: 没有信息可以填充 非null: 填充后的字段信息子类对象
     */
    List<FieldBehavior> fillIn(FieldBehavior fieldBehavior, FieldRelationshipIdTypeEnum fieldRelationshipIdTypeEnum, int id);
}
