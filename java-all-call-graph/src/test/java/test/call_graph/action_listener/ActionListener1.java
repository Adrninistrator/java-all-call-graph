package test.call_graph.action_listener;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

/**
 * @author adrninistrator
 * @date 2022/4/15
 * @description:
 */
public class ActionListener1 implements ActionListener {
    @Override
    public void actionPerformed(ActionEvent e) {
        System.getProperty("");
    }
}
