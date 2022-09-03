package com.adrninistrator.jacg.extensions.code_parser;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.javacg.util.JavaCGUtil;
import org.apache.bcel.classfile.JavaClass;
import org.apache.bcel.classfile.Method;
import org.apache.bcel.generic.Type;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedWriter;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2021/11/5
 * @description: 查找指定父类的非抽象子类满足条件的非抽象方法，基类
 */
public abstract class AbstractFindMethodCodeParser extends AbstractHandleExtendsOrImplCodeParser {
    private static final Logger logger = LoggerFactory.getLogger(AbstractFindMethodCodeParser.class);

    private List<String> childClassMethodList;

    /**
     * 指定查找继承自指定类的子类，还是实现了指定接口的实现类
     *
     * @return true：查找继承自指定类的子类；false：查找实现了指定接口的实现类
     */
    protected abstract boolean findExtendsOrImpl();

    /**
     * 指定父类或接口的类名，用于查找其子类或实现类
     *
     * @return
     */
    protected abstract String getSuperOrInterfaceClassName();

    /**
     * 判断当前方法是否需要记录
     *
     * @param javaClass
     * @param method
     * @param args
     * @return true：需要记录；false：不需要记录
     */
    protected abstract boolean checkMethodInfo(JavaClass javaClass, Method method, Type[] args);

    /**
     * 指定生成结果的文件路径
     *
     * @return
     */
    protected abstract String getResultFilePath();

    @Override
    public void init() {
        childClassMethodList = new ArrayList<>();
    }

    @Override
    public void handleClass(JavaClass javaClass) {
        if (javaClass.isAbstract() || javaClass.isInterface()) {
            return;
        }

        String superOrInterfaceClassName = getSuperOrInterfaceClassName();

        if (findExtendsOrImpl()) {
            // 判断是否继承自指定类
            if (!JavaCGUtil.isChildOf(javaClass.getClassName(), superOrInterfaceClassName, extendsClassMethodInfoMap)) {
                return;
            }
        } else if (!JavaCGUtil.isImplementationOf(javaClass.getClassName(), superOrInterfaceClassName, extendsClassMethodInfoMap, classInterfaceMethodInfoMap)) {
            // 判断是否实现了指定接口
            return;
        }

        for (Method method : javaClass.getMethods()) {
            Type[] args = method.getArgumentTypes();
            // 判断当前方法是否需要记录
            if (!method.isAbstract() && checkMethodInfo(javaClass, method, args)) {
                childClassMethodList.add(JavaCGUtil.formatFullMethod(javaClass.getClassName(), method.getName(), JavaCGUtil.getArgListStr(args)));
            }
        }
    }

    @Override
    public void beforeDone() {
        Collections.sort(childClassMethodList);

        try (BufferedWriter out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(getResultFilePath()), StandardCharsets.UTF_8))) {
            for (String childClassMethod : childClassMethodList) {
                out.write(childClassMethod + JACGConstants.NEW_LINE);
            }
        } catch (Exception e) {
            logger.error("error ", e);
        }
    }
}
