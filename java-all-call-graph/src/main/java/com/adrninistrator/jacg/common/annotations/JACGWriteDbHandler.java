package com.adrninistrator.jacg.common.annotations;

import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.javacg.common.enums.JavaCGOutPutFileTypeEnum;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * @author adrninistrator
 * @date 2023/6/3
 * @description: 写入数据库的类使用的注解
 */
@Target({ElementType.TYPE})
@Retention(RetentionPolicy.RUNTIME)
public @interface JACGWriteDbHandler {

    // 是否需要读取文件
    boolean readFile();

    // 需要读取的文件是属于主要的文件还是其他的文件
    boolean mainFile() default false;

    // 需要读取的主要文件类型
    JavaCGOutPutFileTypeEnum mainFileTypeEnum() default JavaCGOutPutFileTypeEnum.OPFTE_ILLEGAL;

    // 需要读取的其他文件名称
    String otherFileName() default "";

    // 需要读取的文件最小列数
    int minColumnNum() default 0;

    // 需要读取的文件最大列数
    int maxColumnNum() default 0;

    // 需要写到的数据库表信息
    DbTableInfoEnum dbTableInfoEnum();
}
