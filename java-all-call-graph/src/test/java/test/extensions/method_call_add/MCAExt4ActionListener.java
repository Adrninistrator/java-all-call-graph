package test.extensions.method_call_add;

import com.adrninistrator.jacg.extensions.method_call_add.AbstractMethodCallAdd4ExtendsImpl;
import com.adrninistrator.javacg.common.JavaCGConstants;

import java.awt.event.ActionListener;

/**
 * @author adrninistrator
 * @date 2022/11/20
 * @description: 用于添加方法调用关系的扩展类示例，处理ActionListener
 */
public class MCAExt4ActionListener extends AbstractMethodCallAdd4ExtendsImpl {
    @Override
    protected String chooseCalleeSuperClassOrInterfaceName() {
        // ActionListener接口的完整类名
        return ActionListener.class.getName();
    }

    @Override
    protected String chooseCalleeMethodName() {
        // 当ActionListener接口实现类的构造函数被调用时，添加缺失的方法调用关系，该方法名作为调用方法
        return JavaCGConstants.METHOD_NAME_INIT;
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
