package test.callgraph.spring.mvc;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;
import test.callgraph.field.TestField1;
import test.callgraph.field.TestField2;
import test.callgraph.field.cycle.TestUseFieldCycle1;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2022/11/17
 * @description:
 */
@RestController
@RequestMapping(path = "test2")
public class TestSpringController2 {
    @GetMapping(path = "get1")
    public String get1() {
        return "";
    }

    @PostMapping()
    public String post(@RequestBody final String req) {
        return req;
    }

    @PostMapping()
    public List<TestField2> post2(List<TestField1> list) {
        return null;
    }

    @RequestMapping(value = "test1", method = RequestMethod.GET)
    public void test1(TestField1 testField1) {
    }

    @RequestMapping(value = {"test2a", "/test2b", "test2c"}, method = RequestMethod.POST)
    public void test2(TestUseFieldCycle1 testUseFieldCycle1) {
    }
}
