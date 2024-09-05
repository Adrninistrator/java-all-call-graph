package com.adrninistrator.jacg.handler.fieldrelationship.filler;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;

/**
 * @author adrninistrator
 * @date 2023/9/24
 * @description: 为MyBatis的Entity字段行为填充信息的类，支持缓存
 * ！！！当前类不能使用，MyBatis Entity对应的字段可能对应insert、select等不同类型的操作，不能根据类名和字段名进行缓存！！！
 */
public class MyBatisEntityFieldBehaviorCachedFiller extends DefaultFieldBehaviorCachedFiller<MyBatisEntityFieldBehaviorFiller> {

    public MyBatisEntityFieldBehaviorCachedFiller(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
        throw new JavaCG2RuntimeException("当前类不能使用");
    }

    public MyBatisEntityFieldBehaviorCachedFiller(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
        throw new JavaCG2RuntimeException("当前类不能使用");
    }
}
