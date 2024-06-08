package com.adrninistrator.jacg.dto.methodcall.parsed;

/**
 * @author adrninistrator
 * @date 2023/11/9
 * @description: 方法调用信息，解析后的数据，基类
 */
public abstract class AbstractMethodCallInfoParsed {

    // 是否属于等值转换前的数据
    protected final boolean equivalentConversion;

    public AbstractMethodCallInfoParsed() {
        equivalentConversion = false;
    }

    public AbstractMethodCallInfoParsed(boolean equivalentConversion) {
        this.equivalentConversion = equivalentConversion;
    }

    public boolean isEquivalentConversion() {
        return equivalentConversion;
    }
}
