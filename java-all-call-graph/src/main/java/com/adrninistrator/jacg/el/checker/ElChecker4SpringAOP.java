package com.adrninistrator.jacg.el.checker;

import com.adrninistrator.jacg.el.enums.ElConfigEnum;
import com.adrninistrator.jacg.el.manager.ElManager;
import com.adrninistrator.javacg2.el.checker.JavaCG2ElChecker4ParseClass;

/**
 * @author adrninistrator
 * @date 2025/9/27
 * @description:
 */
public class ElChecker4SpringAOP extends JACGElChecker {

    @Override
    protected void jacgDoCheck(ElManager elManager, ElConfigEnum elConfig) {
        elManager.checkIgnoreSpringBean4AOP(JavaCG2ElChecker4ParseClass.CLASS_NAME_EXAMPLE);
    }
}

