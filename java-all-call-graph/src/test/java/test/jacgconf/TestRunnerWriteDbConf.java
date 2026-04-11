package test.jacgconf;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.runner.RunnerWriteDb;
import com.adrninistrator.javacg2.conf.JavaCG2ConfigureWrapper;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;

/**
 * @author adrninistrator
 * @date 2026/2/16
 * @description: 读取jar包内容，生成方法调用关系，并写入数据库
 */

public class TestRunnerWriteDbConf {

    public static void main(String[] args) {
        JavaCG2ConfigureWrapper javaCG2ConfigureWrapper = new JavaCG2ConfigureWrapper(false, ".");
        ConfigureWrapper configureWrapper = new ConfigureWrapper(false, ".");
        boolean success = new RunnerWriteDb(javaCG2ConfigureWrapper, configureWrapper).run();
        if (!success) {
            throw new JavaCG2RuntimeException("执行失败");
        }
    }
}
