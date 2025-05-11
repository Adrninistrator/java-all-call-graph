package com.adrninistrator.jacg.diff.dto.result;

import com.adrninistrator.jacg.diff.dto.method.ModifiedMethodInfo;

import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2025/4/10
 * @description: 处理jar包变化的结果
 */
public class JarDiffResult {

    // 处理成功还是失败
    private boolean success;

    /*
        保存各个jar包中发生变化的方法信息
        key     jar包名称
        value   发生变化的方法信息列表
     */
    private Map<String, List<ModifiedMethodInfo>> jarModifiedMethodInfoMap;

    public static JarDiffResult genFailResult() {
        return new JarDiffResult(false, null);
    }

    public static JarDiffResult genSuccessResult(Map<String, List<ModifiedMethodInfo>> jarModifiedMethodInfoMap) {
        return new JarDiffResult(true, jarModifiedMethodInfoMap);
    }

    public JarDiffResult() {
    }

    public JarDiffResult(boolean success, Map<String, List<ModifiedMethodInfo>> jarModifiedMethodInfoMap) {
        this.success = success;
        this.jarModifiedMethodInfoMap = jarModifiedMethodInfoMap;
    }

    public boolean isSuccess() {
        return success;
    }

    public void setSuccess(boolean success) {
        this.success = success;
    }

    public Map<String, List<ModifiedMethodInfo>> getJarModifiedMethodInfoMap() {
        return jarModifiedMethodInfoMap;
    }

    public void setJarModifiedMethodInfoMap(Map<String, List<ModifiedMethodInfo>> jarModifiedMethodInfoMap) {
        this.jarModifiedMethodInfoMap = jarModifiedMethodInfoMap;
    }
}
