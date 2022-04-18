package test.call_graph.action_listener;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

/**
 * @author adrninistrator
 * @date 2022/4/15
 * @description:
 */
public class TestActionListener {
    public void test1() {
        ActionListener1 testActionListener = new ActionListener1();
        new Button().addActionListener(testActionListener);
    }

    public void test2() {
        new Button().addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                System.out.println("");
            }
        });
    }

    public void test3() {
        new Button().addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                System.setProperty("", "");
            }
        });
    }

    public void test4() {
        new Button().addActionListener(e -> System.setProperty("", ""));
    }
}
