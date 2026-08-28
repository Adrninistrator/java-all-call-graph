package com.adrninistrator.jacg.dto.db;

import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2026/6/16
 * @description: 查询任意数据的结果
 */
public class QueryDataResult {

    // 是否执行成功
    private boolean success;

    // 查询结果，List中每个Map代表一行数据；执行失败时为null
    private List<Map<String, Object>> resultList;

    // 查询耗时（毫秒）
    private long costTimeMs;

    // 是否执行超时
    private boolean timeout;

    // 失败信息
    private String errorMsg;

    public boolean isSuccess() {
        return success;
    }

    public void setSuccess(boolean success) {
        this.success = success;
    }

    public List<Map<String, Object>> getResultList() {
        return resultList;
    }

    public void setResultList(List<Map<String, Object>> resultList) {
        this.resultList = resultList;
    }

    public long getCostTimeMs() {
        return costTimeMs;
    }

    public void setCostTimeMs(long costTimeMs) {
        this.costTimeMs = costTimeMs;
    }

    public boolean isTimeout() {
        return timeout;
    }

    public void setTimeout(boolean timeout) {
        this.timeout = timeout;
    }

    public String getErrorMsg() {
        return errorMsg;
    }

    public void setErrorMsg(String errorMsg) {
        this.errorMsg = errorMsg;
    }

    @Override
    public String toString() {
        return "QueryDataResult{" +
                "success=" + success +
                ", resultListSize=" + (resultList != null ? resultList.size() : "null") +
                ", costTimeMs=" + costTimeMs +
                ", timeout=" + timeout +
                ", errorMsg='" + errorMsg + '\'' +
                '}';
    }
}
