package test.jacgconf;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dto.callstack.CallStackFileResult;
import com.adrninistrator.jacg.findstack.FindCallStackTrace;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;

/**
 * @author adrninistrator
 * @date 2026/2/16
 * @description: 生成包含关键字的方法到起始方法之间的调用堆栈，查看方法向下调用链时使用，按照层级增大的方向显示
 */

public class TestFindCallStackTrace4erConf {

    public static void main(String[] args) {
        ConfigureWrapper configureWrapper = new ConfigureWrapper(false, ".");
        CallStackFileResult result = new FindCallStackTrace(false, configureWrapper).find();
        if (!result.isSuccess()) {
            throw new JavaCG2RuntimeException("执行失败");
        }
    }
}
