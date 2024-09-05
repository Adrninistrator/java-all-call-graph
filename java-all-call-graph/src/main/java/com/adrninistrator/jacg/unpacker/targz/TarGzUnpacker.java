package com.adrninistrator.jacg.unpacker.targz;

import com.adrninistrator.jacg.util.JACGFileUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;
import com.adrninistrator.javacg2.util.JavaCG2FileUtil;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import net.lingala.zip4j.io.inputstream.ZipInputStream;
import net.lingala.zip4j.io.outputstream.ZipOutputStream;
import net.lingala.zip4j.model.LocalFileHeader;
import net.lingala.zip4j.model.ZipParameters;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.tools.tar.TarEntry;
import org.apache.tools.tar.TarInputStream;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2024/1/18
 * @description: 用于解压.tar.gz中的war包、jar包及配置文件
 */
public class TarGzUnpacker extends BaseTarGzUnpacker {

    private static final Logger logger = LoggerFactory.getLogger(TarGzUnpacker.class);

    // 在解压.tar.gz前用于获取jar包中的包名信息
    private final TarGzRecorder tarGzRecorder;

    private final String unpackDestDirPath;
    private final List<String> unpackJarNamePrefixList;
    private final List<String> unpackConfigFileTypeList;

    // 是否对所有的jar包都解压，不管包名是什么
    private final boolean unpackAllPackage;

    // 需要解压其中的.jar的.tar.gz目录名列表，以"/"结尾
    private final List<String> unpackDirWithSeparatorList = new ArrayList<>();

    // 需要解压的jar包名关键字出现在最前面时的列表
    private final List<String> unpackPackageHeadList = new ArrayList<>();
    // 需要解压的jar包名关键字出现在中间时的列表
    private final List<String> unpackPackageWithSeparatorList = new ArrayList<>();
    // 不需要解压的jar包名关键字出现在最前面时的列表
    private final List<String> noUnpackPackageHeadList = new ArrayList<>();
    // 不需要解压的jar包名关键字出现在中间时的列表
    private final List<String> noUnpackPackageWithSeparatorList = new ArrayList<>();

    // 记录各个jar包中的包名前缀的文件路径
    private String jarPackagePrefixFilePath;

    // 记录各个jar包中的包名前缀的Writer
    private BufferedWriter jarPackagePrefixWriter;

    /**
     * @param tarGzFilePath            需要解压的tar.gz文件路径
     * @param unpackDestDirPath        保存解压出文件的目录路径，需要保证执行前该目录不存在或为空
     * @param unpackDirList            需要解压tar.gz文件中的jar文件的目录名列表
     *                                 如 "app"、"WEB-INF/lib"，仅当目录属于tar.gz文件对应目录时认为满足
     *                                 若为空时则对所有的jar包都解压，不判断对应的目录名
     * @param unpackJarNamePrefixList  需要解压的jar文件的名称前缀列表
     *                                 若为空时则所有的jar都解压，不判断对应文件名
     * @param unpackConfigFileTypeList 需要解压的配置文件类型，如 ".xml" ".properties"
     * @param unpackPackageList        当jar包中存在class文件的包名匹配当前列表时，解压对应的class包
     *                                 假如class文件的包名以当前列表中某个元素开头，或包含某个元素，则认为匹配
     *                                 例如包名"aaa.bbb.ccc"可以通过"aaa"、"aaa.bbb"、"aaa.bbb.ccc"、"bbb"、"ccc"匹配，但不能通过"b"匹配
     *                                 若为空时则对所有的class都解压，不判断对应的包名
     * @param noUnpackPackageList      当jar包中的class文件的包名全部匹配当前列表时，不解压对应的class文件
     *                                 包名匹配规则同 unpackPackageList
     *                                 若为空时则对所有的class都解压，不判断对应的包名
     *                                 首先根据class文件的包名和unpackPackageList判断是否需要解压，再根据noUnpackPackageList判断是否不需要解压
     */
    public TarGzUnpacker(String tarGzFilePath,
                         String unpackDestDirPath,
                         List<String> unpackDirList,
                         List<String> unpackJarNamePrefixList,
                         List<String> unpackConfigFileTypeList,
                         List<String> unpackPackageList,
                         List<String> noUnpackPackageList) {
        super(tarGzFilePath);
        this.unpackDestDirPath = unpackDestDirPath;
        this.unpackJarNamePrefixList = unpackJarNamePrefixList;
        this.unpackConfigFileTypeList = unpackConfigFileTypeList;

        if (JavaCG2Util.isCollectionEmpty(unpackPackageList)) {
            unpackAllPackage = true;
            logger.info("所有的jar包无论包名是什么都尝试解压");
        } else {
            unpackAllPackage = false;
            for (String unpackPackage : unpackPackageList) {
                if (!JACGUtil.checkPackageName(unpackPackage)) {
                    throw new IllegalArgumentException("unpackPackageList 中的包名不能以.或/结尾");
                }
                String unpackPackageReplace = JACGUtil.replacePackage2DirPath(unpackPackage);
                unpackPackageHeadList.add(unpackPackageReplace + "/");
                unpackPackageWithSeparatorList.add("/" + unpackPackageReplace + "/");
            }

            if (!JavaCG2Util.isCollectionEmpty(noUnpackPackageList)) {
                for (String noUnpackPackage : noUnpackPackageList) {
                    if (!JACGUtil.checkPackageName(noUnpackPackage)) {
                        throw new IllegalArgumentException("noUnpackPackageList 中的包名不能以.或/结尾");
                    }
                    String noUnpackPackageReplace = JACGUtil.replacePackage2DirPath(noUnpackPackage);
                    noUnpackPackageHeadList.add(noUnpackPackageReplace + "/");
                    noUnpackPackageWithSeparatorList.add("/" + noUnpackPackageReplace + "/");
                }
            }
        }

        if (!JavaCG2Util.isCollectionEmpty(unpackDirList)) {
            for (String unpackDir : unpackDirList) {
                if (StringUtils.endsWithAny(unpackDir, "/", "\\")) {
                    throw new IllegalArgumentException("unpackDirList 中的目录名称不能以/或\\结尾");
                }
                unpackDirWithSeparatorList.add(unpackDir + "/");
            }
        }

        tarGzRecorder = new TarGzRecorder(tarGzFilePath);
    }

    @Override
    protected boolean beforeStart() {
        if (!JavaCG2FileUtil.isDirectoryExists(unpackDestDirPath, true)) {
            logger.error("输出目录不存在且无法创建 {} {}", tarGzFileName, unpackDestDirPath);
            return false;
        }
        File[] files = new File(unpackDestDirPath).listFiles();
        if (!ArrayUtils.isEmpty(files)) {
            logger.error("保存解压后文件的目录非空，请先清空该目录 {}", unpackDestDirPath);
            throw new JavaCG2RuntimeException("保存解压后文件的目录非空，请先清空该目录 " + unpackDestDirPath);
        }

        // 记录.tar.gz中的jar包中的包名前缀信息
        if (!tarGzRecorder.unpack()) {
            return false;
        }

        jarPackagePrefixFilePath = unpackDestDirPath + File.separator + "jar_package_prefix.md";
        try {
            jarPackagePrefixWriter = JavaCG2FileUtil.genBufferedWriter(jarPackagePrefixFilePath);
        } catch (IOException e) {
            logger.error("生成文件失败 {} ", jarPackagePrefixFilePath, e);
            return false;
        }
        return true;
    }

    @Override
    protected void afterDone() {
        logger.info("jar包中的包名前缀保存在文件 {}", jarPackagePrefixFilePath);
        IOUtils.closeQuietly(jarPackagePrefixWriter);
    }

    @Override
    protected boolean handleWarFileInTar(ZipInputStream warInputInTar, TarEntry tarEntry) {
        String warNameOfTarEntry = tarEntry.getName();
        File file = genOutputFile(warNameOfTarEntry);
        try (ZipOutputStream zos = new ZipOutputStream(new FileOutputStream(file))) {
            LocalFileHeader warEntry;
            while ((warEntry = warInputInTar.getNextEntry()) != null) {
                if (warEntry.isDirectory()) {
                    continue;
                }

                String warEntryName = warEntry.getFileName();
                if (checkClassFileType(warEntryName) || checkConfigFileType(warEntryName)) {
                    // 对于.war文件中的.class文件、配置文件，写到生成的.war文件
                    addInput2Jar(warInputInTar, zos, warEntryName);
                } else if (checkJarFileType(warEntryName)) {
                    handleJarFileInTarWar(new ZipInputStream(warInputInTar), warNameOfTarEntry, warEntryName);
                }
            }
            return true;
        } catch (Exception e) {
            logger.error("error {} {} ", tarGzFileName, warNameOfTarEntry, e);
            return false;
        }
    }

    @Override
    protected boolean handleOtherFileInTar(TarInputStream inputStream, TarEntry tarEntry) {
        String tarEntryName = tarEntry.getName();
        if (checkConfigFileType(tarEntryName)) {
            // 假如其他文件在指定的配置文件目录中，且类型匹配，则保存到对应的文件中
            File file = genOutputFile(tarEntryName);
            logger.info("保存配置文件 {} {} {}", tarGzFileName, tarEntryName, file.getAbsolutePath());
            return JACGFileUtil.saveInputToFileNoClose(inputStream, file);
        }
        return true;
    }

    @Override
    protected boolean handleJarFileInTarWar(ZipInputStream jarInputInTarWar, String warNameOfTarEntry, String jarNameOfTarWarEntry) {
        // 判断是否需要解压jar包
        boolean unpackJar = checkNeedUnpackJar(warNameOfTarEntry, jarNameOfTarWarEntry);

        ZipOutputStream zos = null;
        try {
            if (unpackJar) {
                // 需要解压当前jar包
                File file = genOutputFile(jarNameOfTarWarEntry);
                zos = new ZipOutputStream(new FileOutputStream(file));
            }

            LocalFileHeader jarEntry;
            while ((jarEntry = jarInputInTarWar.getNextEntry()) != null) {
                if (jarEntry.isDirectory()) {
                    continue;
                }

                String jarEntryName = jarEntry.getFileName();
                if (unpackJar && (checkClassFileType(jarEntryName) || checkConfigFileType(jarEntryName))) {
                    // 对于需要解压的.jar文件中的.class文件与配置文件，写到生成的.jar文件
                    addInput2Jar(jarInputInTarWar, zos, jarEntryName);
                } else if (checkJarFileType(jarEntryName)) {
                    // spring-boot maven生成的jar包中会存在BOOT-INF/lib/目录，该目录中有其他jar包
                    // 处理jar包中的jar包
                    if (!handleJarFileInJar(new ZipInputStream(jarInputInTarWar), jarNameOfTarWarEntry, jarEntryName)) {
                        return false;
                    }
                }
            }
            return true;
        } catch (Exception e) {
            logger.error("error {} {} {} ", tarGzFileName, warNameOfTarEntry, jarNameOfTarWarEntry, e);
            return false;
        } finally {
            if (zos != null) {
                IOUtils.closeQuietly(zos);
            }
        }
    }

    // 判断是否需要解压jar包
    private boolean checkNeedUnpackJar(String warNameOfTarEntry, String jarNameOfTarWarEntry) {
        Set<String> packagePrefix4Jar;
        if (warNameOfTarEntry == null) {
            // 当前的.war在.tar.gz中
            // 判断.tar.gz中的.jar是否在需要解压的目录中
            if (!checkUnpackJarByDirName(jarNameOfTarWarEntry)) {
                return false;
            }
            // 获取当前jar包中的包名前缀
            packagePrefix4Jar = tarGzRecorder.getPackagePrefix4JarInTar(jarNameOfTarWarEntry);
        } else {
            // 当前的.jar在.tar.gz的.war中
            // 获取当前jar包中的包名前缀
            packagePrefix4Jar = tarGzRecorder.getPackagePrefix4JarInTarWar(warNameOfTarEntry, jarNameOfTarWarEntry);
        }

        // 根据jar包中的包名前缀判断jar包是否需要解压
        if (!checkUnpackJarByPackagePrefix(packagePrefix4Jar, jarNameOfTarWarEntry)) {
            return false;
        }
        // 判断.tar.gz中的.jar文件名前缀是否需要解压
        return checkUnpackJarByJarName(jarNameOfTarWarEntry);
    }

    // 处理jar包中的jar包
    private boolean handleJarFileInJar(ZipInputStream jarInputInJar, String outerJarName, String innerJarName) {
        Set<String> packagePrefix4JarInTarJar = tarGzRecorder.getPackagePrefix4JarInTarJar(outerJarName, innerJarName);
        // 根据jar包中的包名前缀判断jar包是否需要解压
        if (!checkUnpackJarByPackagePrefix(packagePrefix4JarInTarJar, innerJarName)) {
            return true;
        }
        // 判断.tar.gz中的.jar文件名前缀是否需要解压
        if (!checkUnpackJarByJarName(innerJarName)) {
            return true;
        }

        File file = genOutputFile(innerJarName);
        try (ZipOutputStream zos = new ZipOutputStream(new FileOutputStream(file))) {
            LocalFileHeader jarEntry;
            while ((jarEntry = jarInputInJar.getNextEntry()) != null) {
                if (jarEntry.isDirectory()) {
                    continue;
                }

                String jarEntryName = jarEntry.getFileName();
                if (checkClassFileType(jarEntryName) || checkConfigFileType(jarEntryName)) {
                    // 对于.jar文件中的.class文件与配置文件，写到生成的.jar文件
                    addInput2Jar(jarInputInJar, zos, jarEntryName);
                }
            }
            return true;
        } catch (Exception e) {
            logger.error("error {} {} {} ", tarGzFileName, outerJarName, innerJarName, e);
            return false;
        }
    }

    /**
     * 生成需要输出的文件
     *
     * @param entryName
     * @return
     */
    private File genOutputFile(String entryName) {
        // 生成的文件目录保持原有形式
        String filePath = unpackDestDirPath + File.separator + entryName;
        logger.info("保存文件 {} {} {}", tarGzFileName, entryName, filePath);
        File file = new File(filePath);
        if (!JavaCG2FileUtil.isDirectoryExists(file.getParent(), true)) {
            logger.error("创建文件所在目录失败 {}", file.getParent());
            throw new JavaCG2RuntimeException("创建文件所在目录失败 " + file.getParent());
        }
        return file;
    }

    /**
     * 将源压缩包中的文件写入目标jar包中
     *
     * @param inputStream
     * @param targetZos
     * @param entryName
     * @throws IOException
     */
    private void addInput2Jar(InputStream inputStream, ZipOutputStream targetZos, String entryName) throws IOException {
        ZipParameters zipParameters = new ZipParameters();
        zipParameters.setFileNameInZip(entryName);
        targetZos.putNextEntry(zipParameters);
        byte[] data = new byte[8192];
        int len;
        while ((len = inputStream.read(data)) > 0) {
            targetZos.write(data, 0, len);
        }
        targetZos.closeEntry();
    }

    /**
     * 判断指定的文件是否为需要解压的配置文件
     *
     * @param fileName
     * @return
     */
    protected boolean checkConfigFileType(String fileName) {
        for (String unpackConfigFileType : unpackConfigFileTypeList) {
            if (StringUtils.endsWithIgnoreCase(fileName, unpackConfigFileType)) {
                return true;
            }
        }
        return false;
    }

    /**
     * 判断.tar.gz中的.jar是否在需要解压的目录中
     *
     * @param jarNameOfTarWarEntry
     * @return
     */
    private boolean checkUnpackJarByDirName(String jarNameOfTarWarEntry) {
        if (JavaCG2Util.isCollectionEmpty(unpackDirWithSeparatorList)) {
            // 未指定需要解压其中的.jar的.tar.gz目录名列表
            return true;
        }
        // 判断.tar.gz中的.jar是否在需要解压的目录中
        for (String unpackDirWithSeparator : unpackDirWithSeparatorList) {
            if (jarNameOfTarWarEntry.startsWith(unpackDirWithSeparator)) {
                // 仅当.jar在该目录而非子目录中时认为满足
                logger.info("当前jar包在[需要]解压的目录中 {} {} {}", tarGzFileName, jarNameOfTarWarEntry, unpackDirWithSeparator);
                return true;
            }
        }
        return false;
    }

    /**
     * 判断.tar.gz中的.jar文件名前缀是否需要解压
     *
     * @param jarNameOfTarWarEntry
     * @return
     */
    private boolean checkUnpackJarByJarName(String jarNameOfTarWarEntry) {
        if (JavaCG2Util.isCollectionEmpty(unpackJarNamePrefixList)) {
            // 未指定需要解压.jar的文件名前缀
            return true;
        }
        String jarOnlyFileName = JACGFileUtil.getFileNameFromPathInJar(jarNameOfTarWarEntry);
        // 判断.tar.gz中的.jar文件名前缀是否需要解压
        for (String unpackJarNamePrefix : unpackJarNamePrefixList) {
            if (jarOnlyFileName.startsWith(unpackJarNamePrefix)) {
                // 仅当.jar文件使用指定前缀时时认为满足
                logger.info("当前jar包根据前缀判断[需要]解压 {} {} {}", tarGzFileName, jarNameOfTarWarEntry, unpackJarNamePrefix);
                return true;
            }
        }
        logger.info("当前jar包根据前缀判断[不需要]解压 {} {}", tarGzFileName, jarNameOfTarWarEntry);
        return false;
    }

    /**
     * 根据jar包中的包名前缀判断jar包是否需要解压
     *
     * @param jarPackagePrefixSet jar包中的包名前缀
     * @param jarName             jar包名称
     * @return
     */
    private boolean checkUnpackJarByPackagePrefix(Set<String> jarPackagePrefixSet, String jarName) {
        String jarOnlyFileName = JACGFileUtil.getFileNameFromPathInJar(jarName);
        List<String> jarPackagePrefixList = new ArrayList<>(jarPackagePrefixSet);
        if (jarPackagePrefixList.isEmpty()) {
            jarPackagePrefixList.add("");
        }
        try {
            for (String jarPackagePrefix : jarPackagePrefixList) {
                JavaCG2FileUtil.write2FileWithTab(jarPackagePrefixWriter, jarOnlyFileName, jarPackagePrefix, jarName);
            }
        } catch (IOException e) {
            logger.error("写文件失败 {} ", jarPackagePrefixFilePath, e);
        }

        if (unpackAllPackage) {
            return true;
        }
        // 判断当前jar包中的包名是否存在需要解压的包名
        for (String jarPackagePrefix : jarPackagePrefixSet) {
            boolean needUnpack = false;
            for (String unpackPackageHead : unpackPackageHeadList) {
                if (jarPackagePrefix.startsWith(unpackPackageHead)) {
                    // 当前jar包中包含了需要解压的包名
                    needUnpack = true;
                    break;
                }
            }
            for (String unpackPackageWithSeparator : unpackPackageWithSeparatorList) {
                if (jarPackagePrefix.contains(unpackPackageWithSeparator)) {
                    // 当前jar包中包含了需要解压的包名
                    needUnpack = true;
                    break;
                }
            }

            if (!needUnpack) {
                // 当前包名不需要解压
                continue;
            }

            // 当前包名需要解压
            if (noUnpackPackageHeadList.isEmpty()) {
                // 没有指定不需要解压的包名，返回当前jar包需要解压
                logger.info("没有指定不需要解压的包名，当前jar包[需要]解压 {} {}", tarGzFileName, jarName);
                return true;
            }
            boolean notNeedUnpack = false;
            // 有指定不需要解压的包名，判断当前jar包中的包名是否都不需要解压
            for (String noUnpackPackageHead : noUnpackPackageHeadList) {
                if (jarPackagePrefix.startsWith(noUnpackPackageHead)) {
                    notNeedUnpack = true;
                    break;
                }
            }
            if (!notNeedUnpack) {
                for (String noUnpackPackageWithSeparator : noUnpackPackageWithSeparatorList) {
                    if (jarPackagePrefix.contains(noUnpackPackageWithSeparator)) {
                        notNeedUnpack = true;
                        break;
                    }
                }
            }
            if (!notNeedUnpack) {
                logger.info("有指定不需要解压的包名，当前jar包中包含不需要解压的包名之外的包名，当前jar包[需要]解压 {} {} {}", tarGzFileName, jarName, jarPackagePrefix);
                return true;
            }
        }

        logger.info("当前jar包根据包名前缀判断[不需要]解压 {} {}", tarGzFileName, jarName);
        return false;
    }
}
