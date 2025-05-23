package test.jacg;

import com.adrninistrator.jacg.dto.callstack.CallStackFileResult;
import com.adrninistrator.jacg.findstack.FindCallStackTrace;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;

/**
 * @author adrninistrator
 * @date 2021/7/29
 * @description: 生成包含关键字的方法到起始方法之间的调用堆栈，查看方法向下调用链时使用，按照层级增大的方向显示
 */

public class TestFindCallStackTrace4er {

    public static void main(String[] args) {
        CallStackFileResult result = new FindCallStackTrace(false).find();
        if (!result.isSuccess()) {
            throw new JavaCG2RuntimeException("执行失败");
        }
    }
}
