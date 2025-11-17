package test.callgraph.awt;

import org.junit.Test;

import java.awt.Button;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

/**
 * @author adrninistrator
 * @date 2022/4/15
 * @description:
 */
public class TestActionListener {
    @Test
    public void test1a() {
        ActionListener1 testActionListener = new ActionListener1();
        new Button().addActionListener(testActionListener);
        new Button().addActionListener(testActionListener);
    }

    @Test
    public void test1b() {
        ActionListener1 testActionListener = new ActionListener1("a");
        new Button().addActionListener(testActionListener);
        new Button().addActionListener(testActionListener);
    }

    @Test
    public void test2() {
        new Button().addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                System.getProperty("");
            }
        });
    }

    @Test
    public void test3() {
        new Button().addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                new Thread() {
                    @Override
                    public void run() {
                        System.getProperty("");
                    }
                }.start();

                System.setProperty("1", "2");
            }
        });
    }

    @Test
    public void test4() {
        new Thread() {
            @Override
            public void run() {
                new Button().addActionListener(new ActionListener() {
                    @Override
                    public void actionPerformed(ActionEvent e) {
                        System.setProperty("3", "4");
                    }
                });

                System.getProperty("");
            }
        }.start();
    }

    @Test
    public void test5() {
        new Button().addActionListener(e -> System.setProperty("", ""));
    }
}
