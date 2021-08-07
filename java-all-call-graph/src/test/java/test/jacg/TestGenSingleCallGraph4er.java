package test.jacg;

import com.adrninistrator.jacg.other.GenSingleCallGraph;

/**
 * @author adrninistrator
 * @date 2021/7/27
 * @description: 生成某个方法到起始方法之间的调用链，查看方法向下调用链时使用，按照层级增大的方向显示
 */

public class TestGenSingleCallGraph4er {

    public static void main(String[] args) {
        GenSingleCallGraph.setOrder4er();
        GenSingleCallGraph genSingleCallGraph = new GenSingleCallGraph();
        String data = genSingleCallGraph.genCallGraph(args);
        if (data != null) {
            String headerInfo = GenSingleCallGraph.genHeaderInfo(args[0], null);
            System.out.println(headerInfo);
            System.out.println(data);
        }
    }
}
