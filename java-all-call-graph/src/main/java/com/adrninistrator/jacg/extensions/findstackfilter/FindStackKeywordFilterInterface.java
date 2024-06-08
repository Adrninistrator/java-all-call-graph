package com.adrninistrator.jacg.extensions.findstackfilter;

import com.adrninistrator.jacg.dto.callline.CallGraphLineParsed;

/**
 * @author adrninistrator
 * @date 2021/11/5
 * @description: 对方法完整调用链文件搜索关键字时使用的过滤器扩展类接口
 */
public interface FindStackKeywordFilterInterface {

    /**
     * 通过当前行字符串，还是当前行解析后的内容判断是否找到关键字
     *
     * @return true: 通过当前行字符串判断 通过 false: 通过当前行解析后的内容判断
     */
    boolean filterByLine();

    /**
     * 判断在方法完整调用链文件中当前行中是否找到关键字，通过当前行字符串判断
     *
     * @param line 当前行字符串
     * @return true: 找到关键字 false: 未找到关键字
     */
    default boolean filter(String line) {
        return false;
    }

    /**
     * 判断在方法完整调用链文件中当前行中是否找到关键字，通过当前行解析后的内容判断
     *
     * @param callGraphLineParsed 当前行解析后的内容
     * @return true: 找到关键字 false: 未找到关键字
     */
    default boolean filter(CallGraphLineParsed callGraphLineParsed) {
        return false;
    }
}
