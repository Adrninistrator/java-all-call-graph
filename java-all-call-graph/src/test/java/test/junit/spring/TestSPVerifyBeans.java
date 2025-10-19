package test.junit.spring;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import test.junit.base.TestSpringBase;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2022/10/4
 * @description:
 */
public class TestSPVerifyBeans extends TestSpringBase implements ApplicationContextAware {
    private static final Logger logger = LoggerFactory.getLogger(TestSPVerifyBeans.class);

    private ApplicationContext applicationContext;

    @Test
    public void test() {
        List<String> beanNameList = Arrays.asList(applicationContext.getBeanDefinitionNames());
        Collections.sort(beanNameList);

        for (String beanDefinitionName : beanNameList) {
            Object bean = applicationContext.getBean(beanDefinitionName);
            logger.info("bean {} {}", beanDefinitionName, bean.getClass().getName());
        }
    }

    @Override
    public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
        this.applicationContext = applicationContext;
    }
}
