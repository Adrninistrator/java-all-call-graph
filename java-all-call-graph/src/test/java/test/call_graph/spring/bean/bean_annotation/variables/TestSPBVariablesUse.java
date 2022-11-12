package test.call_graph.spring.bean.bean_annotation.variables;

import org.springframework.stereotype.Service;
import test.call_graph.spring.bean.bean_annotation.SpringInterface1;

import javax.annotation.Resource;

/**
 * @author adrninistrator
 * @date 2022/10/4
 * @description:
 */
@Service
public class TestSPBVariablesUse {

    @Resource(name = TestSPBVariablesDefine.BEAN_NAME_1)
    private SpringInterface1 springInterface1A;

    @Resource(name = TestSPBVariablesDefine.BEAN_NAME_2)
    private SpringInterface1 springInterface1B;

    @Resource(name = TestSPBVariablesDefine.BEAN_NAME_3)
    private SpringInterface1 springInterface1C;

    public void test() {
        springInterface1A.test();
        springInterface1B.test();
        springInterface1C.test();
    }
}
