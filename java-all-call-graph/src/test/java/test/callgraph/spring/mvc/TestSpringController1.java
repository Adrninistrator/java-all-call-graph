package test.callgraph.spring.mvc;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;
import test.callgraph.field.cycle.TestUseFieldGenericsCycle1;
import test.callgraph.spring.bean.define.AbstractSpringServiceC;
import test.callgraph.spring.bean.define.SpringInterfaceA;

/**
 * @author adrninistrator
 * @date 2022/11/17
 * @description:
 */
@Controller
@RequestMapping({"test1", "/test1_alias"})
public class TestSpringController1 {
    @Autowired
    private SpringInterfaceA springInterfaceA;

    @Autowired
    @Qualifier("test.callgraph.spring.bean.define.impl.SpringServiceImplC1")
    protected AbstractSpringServiceC springServiceC1;

    @GetMapping("get1")
    @ResponseBody
    public String get1() {
        springInterfaceA.test1();

        springServiceC1.test1();
        post(null);
        return "";
    }

    @PostMapping("post")
    public String post(@RequestBody final String req) {
        System.out.println("");
        springServiceC1.test1();
        return req;
    }

    @RequestMapping(value = "test1", method = RequestMethod.GET)
    public void test1() {
        System.out.println("");
    }

    @RequestMapping(value = {"/test2a", "test2b", "test2c"}, method = RequestMethod.POST)
    public void test2(TestUseFieldGenericsCycle1 testUseFieldGenericsCycle1) {
        System.out.println("");
    }
}
