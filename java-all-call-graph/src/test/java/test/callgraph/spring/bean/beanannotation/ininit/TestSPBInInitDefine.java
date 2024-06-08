package test.callgraph.spring.bean.beanannotation.ininit;

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
public class TestSPBInInitDefine {

    public static final String BEAN_NAME_1 = "testSPBInInitDefine_1";
    public static final String BEAN_NAME_2 = "testSPBInInitDefine_2";

    private SpringInterface1 springInterface1A;
    private final SpringInterface1 springInterface1B;

    public TestSPBInInitDefine() {
        springInterface1B = new SpringServiceImpl1B();
    }

    @Bean(name = BEAN_NAME_1)
    public SpringInterface1 getBean1() {
        springInterface1A = new SpringServiceImpl1A();
        return springInterface1A;
    }

    @Bean(BEAN_NAME_2)
    public SpringInterface1 getBean2() {
        return springInterface1B;
    }
}
