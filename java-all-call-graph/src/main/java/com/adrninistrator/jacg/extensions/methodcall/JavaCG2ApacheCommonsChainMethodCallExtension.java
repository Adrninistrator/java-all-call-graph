package com.adrninistrator.jacg.extensions.methodcall;

/**
 * @author adrninistrator
 * @date 2025/2/22
 * @description: 补充 Apache Commons Chain 的方法调用
 * 为 org.apache.commons.chain.impl.ChainBase#addCommand(org.apache.commons.chain.Command) 方法补充被调用方法
 */
public class JavaCG2ApacheCommonsChainMethodCallExtension extends JavaCG2ObjInstanceMethodNameMCE {

    public static final String TYPE = "ApacheCommonsChain_addCommand";

    @Override
    protected boolean checkHandleCalleeMethod(String calleeClassName, String calleeMethodName, String calleeMethodArgTypes) {
        return "org.apache.commons.chain.impl.ChainBase".equals(calleeClassName) &&
                "addCommand".equals(calleeMethodName) &&
                "(org.apache.commons.chain.Command)".equals(calleeMethodArgTypes);
    }

    @Override
    protected String chooseMethodCallType() {
        return TYPE;
    }
}
