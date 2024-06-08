package com.adrninistrator.jacg.dto.infowithhash;

import com.adrninistrator.jacg.util.JACGUtil;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2023/2/23
 * @description: 通过方法HASH+长度进行关联的信息
 */
public abstract class AbstractInfoWithMethodHash {
    // 方法HASH+长度
    protected String methodHash;

    // 返回当前对应的完整方法
    protected abstract String chooseFullMethod();

    public String getMethodHash() {
        if (methodHash == null) {
            methodHash = JACGUtil.genHashWithLen(chooseFullMethod());
        }
        return methodHash;
    }

    /**
     * 根据相关信息的列表，构建对应的Map，key为方法HASH+长度，value为对应信息
     *
     * @param list
     * @param <T>
     * @return
     */
    public static <T extends AbstractInfoWithMethodHash> Map<String, T> buildMap(List<T> list) {
        Map<String, T> map = new HashMap<>(list.size());
        for (T obj : list) {
            map.put(obj.getMethodHash(), obj);
        }
        return map;
    }
}
