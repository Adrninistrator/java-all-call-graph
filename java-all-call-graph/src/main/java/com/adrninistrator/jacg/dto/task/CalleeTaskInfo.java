package com.adrninistrator.jacg.dto.task;

import java.util.Map;

/**
 * @author adrninistrator
 * @date 2022/5/1
 * @description: 生成向上调用链时，在配置文件中指定的任务信息（Map中的value）
 */
public class CalleeTaskInfo {
    // 是否生成生成方法的调用链
    private boolean genAllMethods;

    /*
        需要生成向上方法调用链的方法信息
        key: 配置文件中指定的任务原始文本
        value: 配置文件中指定的方法名或代码行号
     */
    private Map<String, String> methodInfoMap;

    // 是否有通过名称查找方法
    private boolean findMethodByName;

    public boolean isGenAllMethods() {
        return genAllMethods;
    }

    public void setGenAllMethods(boolean genAllMethods) {
        this.genAllMethods = genAllMethods;
    }

    public Map<String, String> getMethodInfoMap() {
        return methodInfoMap;
    }

    public void setMethodInfoMap(Map<String, String> methodInfoMap) {
        this.methodInfoMap = methodInfoMap;
    }

    public boolean isFindMethodByName() {
        return findMethodByName;
    }

    public void setFindMethodByName(boolean findMethodByName) {
        this.findMethodByName = findMethodByName;
    }
}
