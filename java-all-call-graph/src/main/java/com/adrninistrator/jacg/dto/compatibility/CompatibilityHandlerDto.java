package com.adrninistrator.jacg.dto.compatibility;

import com.adrninistrator.jacg.handler.classes.ClassInfoHandler;
import com.adrninistrator.jacg.handler.extendsimpl.JACGExtendsImplHandler;
import com.adrninistrator.jacg.handler.field.FieldInfoHandler;
import com.adrninistrator.jacg.handler.method.MethodInfoHandler;

/**
 * @author adrninistrator
 * @date 2025/7/20
 * @description: 兼容性检查需要使用的handler类
 */
public class CompatibilityHandlerDto {

    private final ClassInfoHandler classInfoHandler;
    private final MethodInfoHandler methodInfoHandler;
    private final JACGExtendsImplHandler extendsImplHandler;
    private final FieldInfoHandler fieldInfoHandler;

    public CompatibilityHandlerDto(ClassInfoHandler classInfoHandler, MethodInfoHandler methodInfoHandler, JACGExtendsImplHandler extendsImplHandler,
                                   FieldInfoHandler fieldInfoHandler) {
        this.classInfoHandler = classInfoHandler;
        this.methodInfoHandler = methodInfoHandler;
        this.extendsImplHandler = extendsImplHandler;
        this.fieldInfoHandler = fieldInfoHandler;
    }

    public ClassInfoHandler getClassInfoHandler() {
        return classInfoHandler;
    }

    public MethodInfoHandler getMethodInfoHandler() {
        return methodInfoHandler;
    }

    public JACGExtendsImplHandler getExtendsImplHandler() {
        return extendsImplHandler;
    }

    public FieldInfoHandler getFieldInfoHandler() {
        return fieldInfoHandler;
    }
}
