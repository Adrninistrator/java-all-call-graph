package test.callgraph.diffjar.service;

import org.springframework.stereotype.Service;

/**
 * @author adrninistrator
 * @date 2024/4/19
 * @description:
 */
@Service
public class TestService1 {

    public String testA(){
        return "test1-";
    }

    public int testB(){
        return 12;
    }
}
