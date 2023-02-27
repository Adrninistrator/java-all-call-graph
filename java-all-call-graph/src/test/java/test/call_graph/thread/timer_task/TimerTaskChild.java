package test.call_graph.thread.timer_task;

import java.util.TimerTask;

/**
 * @author adrninistrator
 * @date 2022/11/20
 * @description:
 */
public class TimerTaskChild extends TimerTask {
    @Override
    public void run() {
        System.getProperty("");
    }
}
