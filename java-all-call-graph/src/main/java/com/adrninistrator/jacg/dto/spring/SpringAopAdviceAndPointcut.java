package com.adrninistrator.jacg.dto.spring;

import com.adrninistrator.jacg.dto.writedb.WriteDbData4SpringAopAdvice;
import org.springframework.aop.aspectj.AspectJExpressionPointcut;

/**
 * @author adrninistrator
 * @date 2025/7/12
 * @description:
 */
public class SpringAopAdviceAndPointcut {

    private WriteDbData4SpringAopAdvice springAopAdvice;

    private AspectJExpressionPointcut aspectJExpressionPointcut;

    public WriteDbData4SpringAopAdvice getSpringAopAdvice() {
        return springAopAdvice;
    }

    public void setSpringAopAdvice(WriteDbData4SpringAopAdvice springAopAdvice) {
        this.springAopAdvice = springAopAdvice;
    }

    public AspectJExpressionPointcut getAspectJExpressionPointcut() {
        return aspectJExpressionPointcut;
    }

    public void setAspectJExpressionPointcut(AspectJExpressionPointcut aspectJExpressionPointcut) {
        this.aspectJExpressionPointcut = aspectJExpressionPointcut;
    }
}
