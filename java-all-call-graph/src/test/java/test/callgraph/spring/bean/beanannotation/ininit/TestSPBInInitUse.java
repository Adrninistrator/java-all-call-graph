package test.callgraph.spring.bean.beanannotation.ininit;

import org.springframework.stereotype.Service;
import test.callgraph.spring.bean.beanannotation.SpringInterface1;

import javax.annotation.Resource;

/**
 * @author adrninistrator
 * @date 2022/10/4
 * @description:
 */
@Service
public class TestSPBInInitUse {

    @Resource(name = TestSPBInInitDefine.BEAN_NAME_1)
    private SpringInterface1 springInterface1A;

    public void test() {
        springInterface1A.test();
    }
}
