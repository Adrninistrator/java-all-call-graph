package test.extensions.code_parser;

import com.adrninistrator.jacg.extensions.code_parser.AbstractManualAddCallGraphSimpleParser;

import java.awt.event.ActionListener;

/**
 * @author adrninistrator
 * @date 2022/4/21
 * @description:
 */
public class MACGActionListenerParser extends AbstractManualAddCallGraphSimpleParser {
    @Override
    public String chooseTopSuperOrItfClassFullName() {
        // ActionListener接口的完整类名
        return ActionListener.class.getName();
    }

    @Override
    protected boolean chooseExtendsOrImpl() {
        // 在使用ActionListener时，是实现接口的方式，返回false
        return false;
    }

    @Override
    protected String chooseCalleeMethodName() {
        // 当ActionListener接口实现类的构造函数被调用时，添加缺失的方法调用关系，该方法名作为调用方法
        return "<init>";
    }

    @Override
    protected String chooseAddCalleeMethodName() {
        // 添加缺失的方法调用关系，被调用方法为actionPerformed
        return "actionPerformed";
    }

    @Override
    protected String chooseAddCalleeMethodNameArgs() {
        // 添加缺失的方法调用关系，被调用方法参数类型为java.awt.event.ActionEvent
        return "(java.awt.event.ActionEvent)";
    }
}
