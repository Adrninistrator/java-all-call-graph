package com.adrninistrator.jacg.common.list;

import java.util.Collections;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2024/4/27
 * @description: 带有处理结果的List
 */
public class ListWithResult<T> {

    // 处理是否成功
    private final boolean success;

    // 对应的List
    private final List<T> list;

    /**
     * 生成代表失败的对象
     *
     * @param <T>
     * @return
     */
    public static <T> ListWithResult<T> genFail() {
        return new ListWithResult<>(false, null);
    }

    /**
     * 生成代表为空（非失败）的对象
     *
     * @param <T>
     * @return
     */
    public static <T> ListWithResult<T> genEmpty() {
        return new ListWithResult<>(true, Collections.emptyList());
    }

    public ListWithResult(List<T> list) {
        this(true, list);
    }

    private ListWithResult(boolean success, List<T> list) {
        this.success = success;
        this.list = list;
    }

    public boolean isSuccess() {
        return success;
    }

    public List<T> getList() {
        return list;
    }
}
