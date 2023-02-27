package com.adrninistrator.jacg.extensions.extended_data_add;

import com.adrninistrator.jacg.conf.ConfInfo;
import com.adrninistrator.jacg.dto.method.MethodDetail;
import com.adrninistrator.jacg.dto.method_call.ObjArgsInfoInMethodCall;
import com.adrninistrator.jacg.extensions.dto.extened_data.BaseExtendedData;

/**
 * @author adrninistrator
 * @date 2022/12/9
 * @description: 用于根据方法调用信息添加方法调用自定义数据的接口
 */
public interface ExtendedDataAddInterface {
    /**
     * 初始化
     */
    default void initExtendedDataAddInterface() {
    }

    /**
     * 判断当前被调用方法是否需要处理方法调用自定义数据
     *
     * @param callType           方法调用类型
     * @param calleeMethodDetail 被调用方法详细信息
     * @return true: 需要 false: 不需要
     */
    boolean checkNeedHandle(String callType, MethodDetail calleeMethodDetail);

    /**
     * 生成方法调用自定义数据
     *
     * @param callType                方法调用类型
     * @param calleeMethodDetail      被调用方法详细信息
     * @param objArgsInfoInMethodCall 方法调用中被调用对象与参数使用的信息，有可能为null
     * @return
     */
    BaseExtendedData genBaseExtendedData(String callType, MethodDetail calleeMethodDetail, ObjArgsInfoInMethodCall objArgsInfoInMethodCall);

    /**
     * 保存当前使用的参数配置
     *
     * @param confInfo
     */
    default void setConfInfo(ConfInfo confInfo) {
    }
}
