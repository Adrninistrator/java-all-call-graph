package test.jacg;

import com.adrninistrator.jacg.other.GenSingleCallGraph;

/**
 * @author adrninistrator
 * @date 2021/7/29
 * @description: 生成某个方法到起始方法之间的调用链，查看方法向上调用链时使用，按照层级减小的方向显示
 */

public class TestGenSingleCallGraph4ee {

    public static void main(String[] args) {
        GenSingleCallGraph.setOrder4ee();
        GenSingleCallGraph genSingleCallGraph = new GenSingleCallGraph();
        String data = genSingleCallGraph.genCallGraph(args);
        if (data != null) {
            String headerInfo = GenSingleCallGraph.genHeaderInfo(args[0], null);
            System.out.println(headerInfo);
            System.out.println(data);
        }
    }
}
