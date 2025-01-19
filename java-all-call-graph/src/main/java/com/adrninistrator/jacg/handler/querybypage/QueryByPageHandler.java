package com.adrninistrator.jacg.handler.querybypage;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.handler.querybypage.callback.QueryByPageCallBack;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2024/4/26
 * @description: 分页查询公共类
 */
public class QueryByPageHandler {
    private static final Logger logger = LoggerFactory.getLogger(QueryByPageHandler.class);

    /**
     * 分页查询并处理，每查询到一页数据就处理一次
     *
     * @param queryByPageCallBack 分页查询回调实现类
     * @param startId             选择起始ID，后面查询时使用大于判断，因此需要对预期值减1，例如指定的ID从1开始时，需要指定为0
     * @param argsByPage          自定义参数
     * @param <T>
     */
    public static <T> boolean queryAndHandle(QueryByPageCallBack<T> queryByPageCallBack, int startId, Object... argsByPage) {
        try {
            doQuery(false, queryByPageCallBack, startId, argsByPage);
            return true;
        } catch (Exception e) {
            logger.error("分页查询出现异常 {} ", queryByPageCallBack.getClass().getName(), e);
            return false;
        }
    }

    /**
     * 分页查询，结果合并到List中
     *
     * @param queryByPageCallBack 分页查询回调实现类
     * @param startId             选择起始ID，后面查询时使用大于判断，因此需要对预期值减1，例如指定的ID从1开始时，需要指定为0
     * @param argsByPage          自定义参数
     * @param <T>
     * @return 非null: 空列表或查询结果 null: 查询失败
     */
    public static <T> List<T> queryAll2List(QueryByPageCallBack<T> queryByPageCallBack, int startId, Object... argsByPage) {
        try {
            return doQuery(true, queryByPageCallBack, startId, argsByPage);
        } catch (Exception e) {
            logger.error("分页查询出现异常 {} ", queryByPageCallBack.getClass().getName(), e);
            return null;
        }
    }

    private static <T> List<T> doQuery(boolean queryAll2List, QueryByPageCallBack<T> queryByPageCallBack, int startId, Object... argsByPage) throws Exception {
        if (queryByPageCallBack == null) {
            logger.error("未指定 {} 实现类", QueryByPageCallBack.class.getName());
            return null;
        }
        List<T> returnList = null;
        if (queryAll2List) {
            returnList = new ArrayList<>(100);
        }

        // 选择起始ID
        int currentStartId = startId;
        while (true) {
            // 从数据库表分页查询当前处理的结束ID
            int currentEndId = queryByPageCallBack.queryCurrentEndId(currentStartId, argsByPage);
            boolean lastQuery = (currentEndId == JACGConstants.PAGE_QUERY_LAST);
            // 通过类名前缀分页查询Lambda表达式方法调用信息
            List<T> dataList = queryByPageCallBack.queryDataByPage(currentStartId, currentEndId, lastQuery, argsByPage);
            if (!JavaCG2Util.isCollectionEmpty(dataList)) {
                if (queryAll2List) {
                    // 添加当前查询到的数据到结果
                    returnList.addAll(dataList);
                } else {
                    // 处理当前查询到的数据
                    if (!queryByPageCallBack.handleDataList(dataList, argsByPage)) {
                        return null;
                    }
                }
            }
            if (lastQuery || (JavaCG2Util.isCollectionEmpty(dataList) && queryByPageCallBack.exitWhenQueryEmpty())) {
                // 最后一次分页查询，或查询结果为空且需要结束
                break;
            }
            currentStartId = currentEndId;
        }
        return returnList;
    }

    private QueryByPageHandler() {
        throw new IllegalStateException("illegal");
    }
}
