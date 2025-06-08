package test.diffjar.service;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import test.diffjar.dao.TestJarDiffTableMapper;

/**
 * @author adrninistrator
 * @date 2024/4/19
 * @description:
 */
@Service
public class TestService1 {

    private TestJarDiffTableMapper testJarDiffTableMapper;

    public String testA() {
        return "test1-";
    }

    public int testB(){
        return 1;
    }

    public String commandA(){
        return "test1";
    }

    public String commandA2(){
        return "test1";
    }

    public int commandB(){
        return 1;
    }

    public void testMapperModified() {
        testJarDiffTableMapper.selectByPrimaryKey("");
    }

    public void testMapperNotModified() {
        testJarDiffTableMapper.deleteByFlag("");
    }
}
