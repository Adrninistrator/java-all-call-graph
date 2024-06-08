package test.callgraph.spring.bean.beanannotation.directreturn;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import test.callgraph.spring.bean.beanannotation.SpringInterface1;
import test.callgraph.spring.bean.beanannotation.SpringServiceImpl1A;
import test.callgraph.spring.bean.beanannotation.SpringServiceImpl1B;

/**
 * @author adrninistrator
 * @date 2022/10/4
 * @description:
 */
@Configuration
public class TestSPBDirectReturnDefine {

    public static final String BEAN_NAME_2 = "testSPBDirectReturnDefine_2";

    @Bean
    public SpringInterface1 getTestSPBDirectReturnDefineBean1() {
        return new SpringServiceImpl1A();
    }

    @Bean(name = BEAN_NAME_2)
    public SpringInterface1 getBean2() {
        return genBean2();
    }

    private SpringServiceImpl1B genBean2() {
        return new SpringServiceImpl1B();
    }
}
