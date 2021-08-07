package com.adrninistrator.jacg.util;

import java.io.File;

/**
 * @author adrninistrator
 * @date 2021/8/3
 * @description:
 */

public class FileUtilNoLogger {

    /**
     * 判断目录是否存在，不存在时尝试创建
     *
     * @param dirPath 需要判断的目录路径
     * @param tryMake 是否尝试创建目录
     * @return true: 指定路径的目录存在（已存在或新创建），false: 目录不存在（指定路径为文件，或创建失败）
     */
    public static boolean isDirectoryExists(String dirPath, boolean tryMake) {
        File file = new File(dirPath);
        if (file.exists()) {
            if (file.isDirectory()) {
                return true;
            }

            System.err.println("已存在文件: " + dirPath);
            return false;
        }

        if (!tryMake) {
            return false;
        }

        // 目录不存在，则尝试创建
        if (file.mkdirs()) {
            System.out.println("创建目录: " + dirPath);
            return true;
        }

        System.err.println("创建目录失败: " + dirPath);
        return false;
    }
}
