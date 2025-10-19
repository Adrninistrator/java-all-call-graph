package com.adrninistrator.jacg.el.checker;

import com.adrninistrator.jacg.el.enums.ElConfigEnum;
import com.adrninistrator.jacg.el.manager.ElManager;
import com.adrninistrator.javacg2.el.checker.JavaCG2ElChecker4ParseMethod;

/**
 * @author adrninistrator
 * @date 2025/9/28
 * @description:
 */
public class ElChecker4JarDiff extends JACGElChecker {

    @Override
    protected void jacgDoCheck(ElManager elManager, ElConfigEnum elConfig) {
        elManager.checkIgnoreSpringBean4AOP(JavaCG2ElChecker4ParseMethod.FULL_METHOD_EXAMPLE);
    }
}
