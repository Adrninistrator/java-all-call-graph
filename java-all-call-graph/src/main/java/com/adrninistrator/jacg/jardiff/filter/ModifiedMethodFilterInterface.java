package com.adrninistrator.jacg.jardiff.filter;

import com.adrninistrator.jacg.jardiff.dto.method.ModifiedMethodInfo;

/**
 * @author adrninistrator
 * @date 2025/6/8
 * @description: JarDiff功能对找到的发生变化的方法的过滤器
 */
public interface ModifiedMethodFilterInterface {

    boolean skipMethod(ModifiedMethodInfo modifiedMethodInfo);
}
