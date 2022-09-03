package com.adrninistrator.jacg.extensions.extended_data_add;

import com.adrninistrator.jacg.extensions.dto.extened_data.BaseExtendedData;

/**
 * @author adrninistrator
 * @date 2021/11/8
 * @description: 添加自定义数据的自定义处理类接口
 */
public interface ExtendedDataAddInterface {

    /**
     * 初始化
     */
    void init();

    /**
     * 判断当前方法调用是否需要添加自定义数据
     *
     * @param callerFullMethod 调用者完整方法
     * @param calleeFullMethod 被调用者完整方法
     * @return true: 需要，false: 不需要
     */
    boolean checkMethodCall(String callerFullMethod, String calleeFullMethod);

    /**
     * 添加自定义数据
     *
     * @param callerFullMethod  调用者完整方法
     * @param calleeFullMethod  被调用者完整方法
     * @param calleeSeqInCaller 被调用者方法在调用者方法中的出现序号，从1开始
     * @return 被添加的自定义数据
     */
    BaseExtendedData getExtendedData(String callerFullMethod, String calleeFullMethod, long calleeSeqInCaller);
}
