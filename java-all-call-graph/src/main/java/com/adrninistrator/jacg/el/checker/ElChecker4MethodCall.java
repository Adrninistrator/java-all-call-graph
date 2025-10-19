package com.adrninistrator.jacg.el.checker;

import com.adrninistrator.jacg.el.enums.ElConfigEnum;
import com.adrninistrator.jacg.el.manager.ElManager;
import com.adrninistrator.javacg2.common.enums.JavaCG2CallTypeEnum;
import com.adrninistrator.javacg2.el.checker.JavaCG2ElChecker4ParseMethod;

/**
 * @author adrninistrator
 * @date 2025/8/21
 * @description:
 */
public class ElChecker4MethodCall extends JACGElChecker {

    @Override
    protected void jacgDoCheck(ElManager elManager, ElConfigEnum elConfig) {
        elManager.checkIgnoreMethodCall(JavaCG2CallTypeEnum.values()[0].getType(), JavaCG2ElChecker4ParseMethod.FULL_METHOD_EXAMPLE,
                JavaCG2ElChecker4ParseMethod.FULL_METHOD_EXAMPLE, 0);
    }
}
