package com.adrninistrator.jacg.jardiff.dto.result;

import com.adrninistrator.jacg.jardiff.dto.method.ModifiedMethodInfo;

import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2025/4/10
 * @description: 处理jar文件变化的结果
 */
public class JarDiffResult {

    // 处理成功还是失败
    private final boolean success;

    /*
        保存各个jar文件中发生变化的方法信息
        key     jar文件名称
        value   发生变化的方法信息列表
     */
    private final Map<String, List<ModifiedMethodInfo>> jarModifiedMethodInfoMap;

    public static JarDiffResult genFailResult() {
        return new JarDiffResult(false, null);
    }

    public static JarDiffResult genSuccessResult(Map<String, List<ModifiedMethodInfo>> jarModifiedMethodInfoMap) {
        return new JarDiffResult(true, jarModifiedMethodInfoMap);
    }

    public JarDiffResult(boolean success, Map<String, List<ModifiedMethodInfo>> jarModifiedMethodInfoMap) {
        this.success = success;
        this.jarModifiedMethodInfoMap = jarModifiedMethodInfoMap;
    }

    public boolean isSuccess() {
        return success;
    }

    public Map<String, List<ModifiedMethodInfo>> getJarModifiedMethodInfoMap() {
        return jarModifiedMethodInfoMap;
    }
}
