package test.callgraph.thread.timertask;

import java.util.Timer;
import java.util.TimerTask;

/**
 * @author adrninistrator
 * @date 2022/11/20
 * @description:
 */
public class TestTimerTask {
    public void test1() {
        TimerTask timerTask = new TimerTask() {
            @Override
            public void run() {
                System.out.println("");
            }
        };
        Timer timer = new Timer("test1", false);
        timer.schedule(timerTask, 1000L);
    }

    public void test2() {
        Timer timer = new Timer("test2", false);
        timer.schedule(new TimerTaskChild(), 1000L);
    }

    public void test3() {
        Timer timer = new Timer("test3", false);
        timer.schedule(new TimerTaskChild(), 1000L);
    }
}
