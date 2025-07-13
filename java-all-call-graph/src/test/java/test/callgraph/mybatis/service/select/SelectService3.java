package test.callgraph.mybatis.service.select;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

/**
 * @author adrninistrator
 * @date 2023/10/23
 * @description: select语句对应的返回字段
 */
@Service
public class SelectService3 {
    private static final Logger logger = LoggerFactory.getLogger(SelectService3.class);

    private SelectService2 selectService2;

    public String useInOtherMethod1(String data) {
        return selectService2.useInOtherMethod1(data);
    }
}
