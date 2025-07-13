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
        return 12;
    }

    public String commandA(){
        return "test1-";
    }

    public String commandA2(){
        return "test1-";
    }

    public int commandB(){
        return 12;
    }

    public void testMapperModified() {
        testJarDiffTableMapper.selectByPrimaryKey("");
    }

    public void testMapperModified2() {
        testJarDiffTableMapper.select2("");
    }

    public void testMapperNotModified1() {
        testJarDiffTableMapper.selectByPrimaryKeySame("");
    }

    public void testMapperNotModified2() {
        testJarDiffTableMapper.deleteByFlag("");
    }
    public void testMapperNew() {
        testJarDiffTableMapper.selectByPrimaryKey2("");
    }
}
