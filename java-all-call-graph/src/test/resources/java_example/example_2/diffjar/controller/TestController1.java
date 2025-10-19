package test.diffjar.controller;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import test.diffjar.service.TestService1;

/**
 * @author adrninistrator
 * @date 2024/4/19
 * @description:
 */
@RestController
@RequestMapping("test1")
public class TestController1 {

    private static final Logger logger = LoggerFactory.getLogger(TestController1.class);

    @Autowired
    private TestService1 testService1;

    @GetMapping("get1")
    public String get1() {
        return test();
    }

    @GetMapping("get2")
    public String get2() {
        return "get-";
    }

    @PostMapping("post")
    public String post(@RequestBody final String req) {
        logger.info("req- {}", req);
        return String.valueOf(testService1.testB());
    }

    private String test() {
        return testService1.testA();
    }
}
