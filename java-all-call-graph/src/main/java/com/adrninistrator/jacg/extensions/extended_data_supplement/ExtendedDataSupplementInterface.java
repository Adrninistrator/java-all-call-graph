package com.adrninistrator.jacg.extensions.extended_data_supplement;

/**
 * @author adrninistrator
 * @date 2021/10/20
 * @description:
 */
public interface ExtendedDataSupplementInterface {

    /**
     * 初始化
     */
    void init();

    /**
     * 返回当前处理类处理的自定义数据类型
     *
     * @return
     */
    String getDataType();

    /**
     * 对自定义数据进行补充，如果补充成功，则返回补充后的数据；如果补充不成功，则返回原始数据
     *
     * @param dataValue 补充前的自定义数据
     * @return 补充后的自定义数据
     */
    String supplement(String dataValue);
}
