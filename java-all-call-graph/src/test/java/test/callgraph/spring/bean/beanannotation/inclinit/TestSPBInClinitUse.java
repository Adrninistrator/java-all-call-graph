package test.callgraph.spring.bean.beanannotation.inclinit;

import org.springframework.stereotype.Service;
import test.callgraph.spring.bean.beanannotation.SpringInterface1;

import javax.annotation.Resource;

/**
 * @author adrninistrator
 * @date 2022/10/4
 * @description:
 */
@Service
public class TestSPBInClinitUse {

    @Resource(name = TestSPBInClinitDefine.BEAN_NAME_1)
    private SpringInterface1 springInterface1A;

    @Resource(name = TestSPBInClinitDefine.BEAN_NAME_2)
    private SpringInterface1 springInterface1B;

    public void test() {
        springInterface1A.test();
        springInterface1B.test();
    }
}
