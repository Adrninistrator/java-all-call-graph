package com.adrninistrator.jacg.el.checker;

import com.adrninistrator.jacg.el.enums.ElConfigEnum;
import com.adrninistrator.jacg.el.manager.ElManager;
import com.adrninistrator.javacg2.el.checker.AbstractElChecker;
import com.adrninistrator.javacg2.el.enums.interfaces.ElConfigInterface;
import com.adrninistrator.javacg2.el.manager.AbstractElManager;
import com.adrninistrator.javacg2.el.util.ElUtil;

/**
 * @author adrninistrator
 * @date 2025/8/21
 * @description: 当前项目使用的表达式检查抽象父类
 */
public abstract class JACGElChecker extends AbstractElChecker {

    @Override
    protected void doCheck(AbstractElManager elManager, ElConfigInterface elConfig) {
        try {
            // 设置执行用于检测的表达式标志
            ElUtil.setRunInCheckerFlag();
            jacgDoCheck((ElManager) elManager, (ElConfigEnum) elConfig);
        } finally {
            // 清理执行用于检测的表达式标志
            ElUtil.clearRunInCheckerFlag();
        }
    }

    /**
     * 执行检查表达式
     *
     * @param elManager
     * @param elConfig
     */
    protected abstract void jacgDoCheck(ElManager elManager, ElConfigEnum elConfig);
}

