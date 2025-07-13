package test.callgraph.spring.aop.multi.aspect;

import org.aspectj.lang.JoinPoint;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class TestSpringAOPXmlLoggingAspectMulti2Order40 {
    private static final Logger logger = LoggerFactory.getLogger(TestSpringAOPXmlLoggingAspectMulti2Order40.class);

    // 前置通知
    public void logBefore(JoinPoint joinPoint) {
        logger.info("Before method: {}", joinPoint.getSignature().getName());
    }

    // 后置通知
    public void logAfter(JoinPoint joinPoint) {
        logger.info("After method: {}", joinPoint.getSignature().getName());
    }

    // 返回后通知
    public void logAfterReturning(JoinPoint joinPoint, Object result) {
        logger.info("AfterReturning method: {}, result: {}", joinPoint.getSignature().getName(), result);
    }

    // 异常通知
    public void logAfterThrowing(JoinPoint joinPoint, Throwable error) {
        logger.error("AfterThrowing in method: {} , exception: {}", joinPoint.getSignature().getName(), error);
    }
}