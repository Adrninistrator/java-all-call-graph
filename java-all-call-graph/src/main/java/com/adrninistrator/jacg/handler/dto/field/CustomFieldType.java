package com.adrninistrator.jacg.handler.dto.field;

import java.util.Set;

/**
 * @author adrninistrator
 * @date 2023/10/4
 * @description: 自定义的字段类型
 */
public class CustomFieldType {

    // 是否已完成数据初始化
    private boolean inited = false;

    // 自定义的字段类型Set
    private Set<String> customFieldTypeSet;

    public boolean isInited() {
        return inited;
    }

    public void setInited(boolean inited) {
        this.inited = inited;
    }

    public Set<String> getCustomFieldTypeSet() {
        return customFieldTypeSet;
    }

    public void setCustomFieldTypeSet(Set<String> customFieldTypeSet) {
        this.customFieldTypeSet = customFieldTypeSet;
    }
}
