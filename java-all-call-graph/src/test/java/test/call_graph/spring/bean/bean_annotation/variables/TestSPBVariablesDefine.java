package test.call_graph.spring.bean.bean_annotation.variables;

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
public class TestSPBVariablesDefine {

    public static final String BEAN_NAME_1 = "testSPBVariablesDefine_1";
    public static final String BEAN_NAME_2 = "testSPBVariablesDefine_2";
    public static final String BEAN_NAME_3 = "testSPBVariablesDefine_3";

    @Bean(name = BEAN_NAME_1)
    public SpringInterface1 getBean1() {
        SpringServiceImpl1A springServiceImpl1A = new SpringServiceImpl1A();
        SpringServiceImpl1B springServiceImpl1B = new SpringServiceImpl1B();
        int i = (int) System.currentTimeMillis() % 10;
        if (i == 1) {
            return springServiceImpl1A;
        }
        return springServiceImpl1B;
    }

    @Bean(name = BEAN_NAME_2)
    public SpringInterface1 getBean2() {
        SpringServiceImpl1A springServiceImpl1A = new SpringServiceImpl1A();
        SpringInterface1 springInterface1 = springServiceImpl1A;
        int i = (int) System.currentTimeMillis() % 10;
        if (i == 1) {
            return springInterface1;
        }
        return new SpringServiceImpl1B();
    }

    @Bean(BEAN_NAME_3)
    public SpringInterface1 getBean3() {
        SpringServiceImpl1B springServiceImpl1B = genBean2();
        int i = (int) System.currentTimeMillis() % 10;
        if (i == 1) {
            return springServiceImpl1B;
        }
        return new SpringServiceImpl1A();
    }

    private SpringServiceImpl1B genBean2() {
        return new SpringServiceImpl1B();
    }
}
