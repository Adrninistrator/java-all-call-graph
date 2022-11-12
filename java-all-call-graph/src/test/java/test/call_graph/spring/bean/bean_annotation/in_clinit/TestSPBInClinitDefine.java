package test.call_graph.spring.bean.bean_annotation.in_clinit;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import test.call_graph.spring.bean.bean_annotation.SpringInterface1;
import test.call_graph.spring.bean.bean_annotation.SpringServiceImpl1A;
import test.call_graph.spring.bean.bean_annotation.SpringServiceImpl1B;

/**
 * @author adrninistrator
 * @date 2022/10/4
 * @description:
 */
@Configuration
public class TestSPBInClinitDefine {

    public static final String BEAN_NAME_1 = "testSPBInClinitDefine_1";
    public static final String BEAN_NAME_2 = "testSPBInClinitDefine_2";

    private static SpringInterface1 SPRING_INTERFACE1;
    private static final SpringInterface1 SPRING_INTERFACE2 = new SpringServiceImpl1B();

    @Bean(BEAN_NAME_1)
    public SpringInterface1 getBean1() {
        SPRING_INTERFACE1 = new SpringServiceImpl1A();
        return SPRING_INTERFACE1;
    }

    @Bean(name = BEAN_NAME_2)
    public SpringInterface1 getBean2() {
        return SPRING_INTERFACE2;
    }
}
