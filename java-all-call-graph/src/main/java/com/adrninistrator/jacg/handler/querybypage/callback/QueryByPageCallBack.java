package com.adrninistrator.jacg.handler.querybypage.callback;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2024/4/26
 * @description: 分页查询回调接口
 */
public interface QueryByPageCallBack<T> {

    /**
     * 查询本次分页查询的结束ID
     *
     * @param currentStartId 当前起始ID（大于）
     * @param argsByPage     自定义参数
     * @return 查询失败时返回: JACGConstants.PAGE_QUERY_FAIL 查询结束时返回: JACGConstants.PAGE_QUERY_LAST 其他情况返回实际的结束ID
     */
    default int queryCurrentEndId(int currentStartId, Object... argsByPage) {
        return 0;
    }

    /**
     * 根据当前开始及结束ID，分页查询需要的数据
     *
     * @param currentStartId 当前起始ID（大于）
     * @param currentEndId   当前结束ID（小于等于）
     * @param lastQuery      是否最后一次查询
     * @param argsByPage     自定义参数
     * @return
     */
    List<T> queryDataByPage(int currentStartId, int currentEndId, boolean lastQuery, Object... argsByPage);

    /**
     * 处理当前查询到的一页数据
     *
     * @param dataList   当前查询到的一页数据
     * @param argsByPage 自定义参数
     * @return true: 处理成功 false: 处理失败
     */
    default boolean handleDataList(List<T> dataList, Object... argsByPage) throws Exception {
        return true;
    }

    /**
     * 当查询结果为空时是否结束循环查询
     *
     * @return true: 结束 false: 不结束
     */
    default boolean exitWhenQueryEmpty() {
        return false;
    }
}
