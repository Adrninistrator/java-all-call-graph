package test.jacg;

import com.adrninistrator.jacg.runner.RunnerWriteDb;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;

/**
 * @author adrninistrator
 * @date 2021/6/23
 * @description: 读取jar包内容，生成方法调用关系，并写入数据库
 */

public class TestRunnerWriteDb {

    public static void main(String[] args) {
        boolean success = new RunnerWriteDb().run();
        if (!success) {
            throw new JavaCG2RuntimeException("执行失败");
        }
    }
}
