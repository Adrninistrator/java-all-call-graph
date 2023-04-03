package test.call_graph.action_listener;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

/**
 * @author adrninistrator
 * @date 2022/4/15
 * @description:
 */
public class ActionListener1 implements ActionListener {

    private String data;

    public ActionListener1() {
    }

    public ActionListener1(String data) {
        this.data = data;
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        System.getProperty(data);
    }
}
