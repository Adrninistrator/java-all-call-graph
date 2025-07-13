package test.callgraph.spring.aop.customanno1.aspect;

import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.After;
import org.aspectj.lang.annotation.AfterReturning;
import org.aspectj.lang.annotation.AfterThrowing;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

@Aspect
@Component
public class TestSpringAOPCustomAnnoLoggingAspect1B {
    private static final Logger logger = LoggerFactory.getLogger(TestSpringAOPCustomAnnoLoggingAspect1B.class);

    // 前置通知
    @Before("@annotation(test.callgraph.spring.aop.customanno1.annotation.TestSpringAOPCustomAnnotation1B)")
    public void logBefore(JoinPoint joinPoint) {
        logger.info("Before method: {}", joinPoint.getSignature().getName());
    }

    // 后置通知
    @After("@annotation(test.callgraph.spring.aop.customanno1.annotation.TestSpringAOPCustomAnnotation1B)")
    public void logAfter(JoinPoint joinPoint) {
        logger.info("After method: {}", joinPoint.getSignature().getName());
    }

    // 返回后通知
    @AfterReturning(pointcut = "@annotation(test.callgraph.spring.aop.customanno1.annotation.TestSpringAOPCustomAnnotation1B)",
            returning = "result")
    public void logAfterReturning(JoinPoint joinPoint, Object result) {
        logger.info("AfterReturning method: {}, result: {}", joinPoint.getSignature().getName(), result);
    }

    // 异常通知
    @AfterThrowing(pointcut = "@annotation(test.callgraph.spring.aop.customanno1.annotation.TestSpringAOPCustomAnnotation1B)",
            throwing = "error")
    public void logAfterThrowing(JoinPoint joinPoint, Throwable error) {
        logger.error("AfterThrowing in method: {} , exception: {}", joinPoint.getSignature().getName(), error);
    }

    // 环绕通知
    @Around("@annotation(test.callgraph.spring.aop.customanno1.annotation.TestSpringAOPCustomAnnotation1B)")
    public Object logAround(ProceedingJoinPoint joinPoint) throws Throwable {
        logger.info("Around before method: {}", joinPoint.getSignature().getName());
        Object result = joinPoint.proceed();
        logger.info("Around after method: {}", joinPoint.getSignature().getName());
        return result;
    }
}