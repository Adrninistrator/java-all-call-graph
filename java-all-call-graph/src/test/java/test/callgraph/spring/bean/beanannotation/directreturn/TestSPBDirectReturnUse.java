package test.callgraph.spring.bean.beanannotation.directreturn;

import org.springframework.stereotype.Service;
import test.callgraph.spring.bean.beanannotation.SpringInterface1;

import javax.annotation.Resource;

/**
 * @author adrninistrator
 * @date 2022/10/4
 * @description:
 */
@Service
public class TestSPBDirectReturnUse {

    @Resource(name = "getTestSPBDirectReturnDefineBean1")
    private SpringInterface1 springInterface1A;

    @Resource(name = TestSPBDirectReturnDefine.BEAN_NAME_2)
    private SpringInterface1 springInterface1B;

    public void test() {
        springInterface1A.test();
        springInterface1B.test();
    }
}
