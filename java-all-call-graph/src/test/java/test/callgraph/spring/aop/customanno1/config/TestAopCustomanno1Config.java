package test.callgraph.spring.aop.customanno1.config;

import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.EnableAspectJAutoProxy;

/**
 * @author adrninistrator
 * @date 2025/6/11
 * @description:
 */
@Configuration
// xml中有指定包扫描路径，这里可以不指定@ComponentScan
@ComponentScan("test.callgraph.spring.aop.customanno1")
@EnableAspectJAutoProxy
public class TestAopCustomanno1Config {
}
