package com.adrninistrator.jacg.handler.fieldrelationship.filler;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.handler.common.enums.FieldRelationshipIdTypeEnum;
import com.adrninistrator.jacg.handler.dto.field.FieldBehavior;
import com.adrninistrator.jacg.util.field.FieldBehaviorUtil;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2023/10/11
 * @description: 默认的支持缓存的字段行为填充信息的类
 */
public abstract class DefaultFieldBehaviorCachedFiller<T extends FieldBehaviorFillerInterface> extends BaseHandler implements FieldBehaviorFillerInterface {

    /*
        缓存已处理的字段行为Map
        key
            假如对应的类不是MyBatis的Entity，则key使用类名
            假如对应的类是MyBatis的Entity，则key使用类名+字段名
        value
            字段行为列表
     */
    private Map<String, List<FieldBehavior>> cachedFieldBehaviorMap;

    // 实际使用的字段行为填充类，在子类构建函数中需要实例化
    protected T actualFieldBehaviorFiller;

    public DefaultFieldBehaviorCachedFiller(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
    }

    public DefaultFieldBehaviorCachedFiller(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
    }

    @Override
    public void init() {
        if (actualFieldBehaviorFiller == null) {
            throw new JavaCG2RuntimeException("未设置实际使用的字段行为填充类");
        }
        if (cachedFieldBehaviorMap != null) {
            throw new JavaCG2RuntimeException("为了避免被分析的不同应用间的数据有冲突，当前类需要在处理每个应用时创建新的实例");
        }
        cachedFieldBehaviorMap = new HashMap<>();
    }

    @Override
    public List<FieldBehavior> fillIn(FieldBehavior fieldBehavior, FieldRelationshipIdTypeEnum fieldRelationshipIdTypeEnum, int id) {
        // 判断当前类已缓存的数据是否为代表不存在可填充字段行为的列表
        if (FieldBehaviorUtil.checkNoneFieldBehaviorList(cachedFieldBehaviorMap.get(fieldBehavior.getClassName()))) {
            // 当前类已缓存的数据是代表不存在可填充字段行为的列表，返回null
            return null;
        }

        String classAndFieldName = fieldBehavior.genClassAndFieldName();
        List<FieldBehavior> fieldBehaviorList = cachedFieldBehaviorMap.get(classAndFieldName);
        if (fieldBehaviorList != null) {
            // 若已存在则直接返回缓存的值
            return fieldBehaviorList;
        }
        // 若不存在则调用父类方法获取并缓存
        fieldBehaviorList = actualFieldBehaviorFiller.fillIn(fieldBehavior, fieldRelationshipIdTypeEnum, id);
        if (fieldBehaviorList == null) {
            // 若没有信息可以填充，则使用代表不存在可填充字段行为的列表缓存，key使用类名
            cachedFieldBehaviorMap.put(fieldBehavior.getClassName(), FieldBehaviorUtil.NONE_FIELD_BEHAVIOR_LIST);
        } else {
            // 有信息可以填充，key使用类名+字段名
            cachedFieldBehaviorMap.put(classAndFieldName, fieldBehaviorList);
        }
        return fieldBehaviorList;
    }

    @Override
    public void close() {
        super.close();
        // 在close方法中调用实际使用的字段行为填充类的close方法
        if (actualFieldBehaviorFiller instanceof BaseHandler) {
            ((BaseHandler) actualFieldBehaviorFiller).close();
        }
    }
}
