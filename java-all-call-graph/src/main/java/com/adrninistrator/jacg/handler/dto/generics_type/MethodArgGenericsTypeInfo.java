package com.adrninistrator.jacg.handler.dto.generics_type;

import com.fasterxml.jackson.annotation.JsonIgnore;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/3/21
 * @description: 方法参数泛型类型信息
 */
public class MethodArgGenericsTypeInfo extends HashMap<Integer, GenericsTypeValue> {
    /*
        方法参数泛型类型Map
        key
            对应的参数序号，从0开始
        value
            对应的泛型类型
     */

    /**
     * 返回排序后的参数序号列表
     *
     * @return
     */
    @JsonIgnore
    public List<Integer> getArgSeqList() {
        List<Integer> list = new ArrayList<>(keySet());
        Collections.sort(list);
        return list;
    }

    /**
     * 获取指定参数序号对应的泛型类型
     *
     * @param argSeq
     * @return
     */
    public GenericsTypeValue getTypeValue(int argSeq) {
        return get(argSeq);
    }

    /**
     * 设置指定参数序号对应的泛型类型
     *
     * @param argSeq
     * @param genericsTypeList
     */
    public void putTypeValue(int argSeq, GenericsTypeValue genericsTypeList) {
        put(argSeq, genericsTypeList);
    }
}
