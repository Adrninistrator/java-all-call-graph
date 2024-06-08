package com.adrninistrator.jacg.unpacker.targz;

import com.adrninistrator.jacg.util.JACGFileUtil;
import com.adrninistrator.javacg.exceptions.JavaCGRuntimeException;
import com.adrninistrator.javacg.util.JavaCGFileUtil;
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
     * @param unpackDirList            需要解压其中的.jar的.tar.gz目录名列表，如 "app"，仅当目录在.tar.gz根路径，且.jar在该目录而非子目录中时认为满足
     * @param unpackConfigFileTypeList 需要解压的配置文件类型，如 ".xml" ".properties"
     * @param unpackPackageList        当jar包中存在class文件的包名对应路径匹配当前列表时，解压对应的jar包
     *                                 在该参数中使用"/"作为分隔符，如 "aaa"、"a/b"
     *                                 假如当前参数指定了"abc"，jar包中某个class文件的包名对应路径为a/abc/b，则认为匹配
     *                                 若包含了"*all*"则对所有的jar包都解压
     * @param noUnpackPackageList      当jar包中的class文件的包名全部匹配当前列表时，不解压对应的class文件到jar包中，使用"/"作为分隔符，如 "aaa"、"a/b"
     */
    public TarGzUnpacker(String tarGzFilePath,
                         String unpackDestDirPath,
                         List<String> unpackDirList,
                         List<String> unpackConfigFileTypeList,
                         List<String> unpackPackageList,
                         List<String> noUnpackPackageList) {
        super(tarGzFilePath);
        this.unpackDestDirPath = unpackDestDirPath;
        this.unpackConfigFileTypeList = unpackConfigFileTypeList;

        if (unpackPackageList.contains(UNPACK_ALL_PACKAGE_FLAG)) {
            unpackAllPackage = true;
        } else {
            unpackAllPackage = false;
            for (String unpackPackage : unpackPackageList) {
                if (unpackPackage.endsWith("/")) {
                    throw new IllegalArgumentException("unpackPackageList 中的包名不能以/结尾");
                }
                unpackPackageHeadList.add(unpackPackage + "/");
                unpackPackageWithSeparatorList.add("/" + unpackPackage + "/");
            }
            for (String noUnpackPackage : noUnpackPackageList) {
                if (noUnpackPackage.endsWith("/")) {
                    throw new IllegalArgumentException("noUnpackPackageList 中的包名不能以/结尾");
                }
                noUnpackPackageHeadList.add(noUnpackPackage + "/");
                noUnpackPackageWithSeparatorList.add("/" + noUnpackPackage + "/");
            }
        }

        if (unpackDirList != null) {
            for (String unpackDir : unpackDirList) {
                unpackDirWithSeparatorList.add(unpackDir + "/");
            }
        }

        tarGzRecorder = new TarGzRecorder(tarGzFilePath);
    }

    @Override
    protected boolean beforeStart() {
        if (!JavaCGFileUtil.isDirectoryExists(unpackDestDirPath, true)) {
            logger.error("输出目录不存在且无法创建 {} {}", tarGzFileName, unpackDestDirPath);
            return false;
        }
        File[] files = new File(unpackDestDirPath).listFiles();
        if (!ArrayUtils.isEmpty(files)) {
            logger.error("保存解压后文件的目录非空，请先清空该目录 {}", unpackDestDirPath);
            throw new JavaCGRuntimeException("保存解压后文件的目录非空，请先清空该目录 " + unpackDestDirPath);
        }

        // 记录.tar.gz中的jar包中的包名前缀信息
        if (!tarGzRecorder.unpack()) {
            return false;
        }

        jarPackagePrefixFilePath = unpackDestDirPath + File.separator + "jar_package_prefix.md";
        try {
            jarPackagePrefixWriter = JavaCGFileUtil.genBufferedWriter(jarPackagePrefixFilePath);
        } catch (IOException e) {
            logger.error("生成文件失败 {} ", jarPackagePrefixFilePath, e);
            return false;
        }
        return true;
    }

    @Override
    protected void afterDone() {
        logger.info("jar包中的包名前缀保存在文件 {} 包名层级为 {}", jarPackagePrefixFilePath, packagePrefixLevel);
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
                if ((checkClassFileType(warEntryName) || checkConfigFileType(warEntryName))) {
                    // 对于.war文件中的.class文件与配置文件，写到生成的.war文件
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
        boolean unpackJar = false;
        if (warNameOfTarEntry == null) {
            // 当前的.jar在.tar.gz中
            int jarNameLevel = StringUtils.split(jarNameOfTarWarEntry, "/").length;
            // 判断.tar.gz中的.jar是否在需要解压的目录中
            for (String unpackDirWithSeparator : unpackDirWithSeparatorList) {
                if (jarNameOfTarWarEntry.startsWith(unpackDirWithSeparator) && jarNameLevel == 2) {
                    // 仅当.jar在该目录而非子目录中时认为满足
                    logger.info("当前jar包在[需要]解压的目录中 {} {} {}", tarGzFileName, jarNameOfTarWarEntry, unpackDirWithSeparator);
                    unpackJar = true;
                    break;
                }
            }
            if (!unpackJar) {
                Set<String> packagePrefix4JarInTar = tarGzRecorder.getPackagePrefix4JarInTar(jarNameOfTarWarEntry);
                // 根据jar包中的包名前缀判断jar包是否需要解压
                if (checkUnpackJarByPackagePrefix(packagePrefix4JarInTar, jarNameOfTarWarEntry)) {
                    unpackJar = true;
                }
            }
        } else {
            // 当前的.jar在.tar.gz的.war中
            Set<String> packagePrefix4JarInTarWar = tarGzRecorder.getPackagePrefix4JarInTarWar(warNameOfTarEntry, jarNameOfTarWarEntry);
            // 根据jar包中的包名前缀判断jar包是否需要解压
            if (checkUnpackJarByPackagePrefix(packagePrefix4JarInTarWar, jarNameOfTarWarEntry)) {
                unpackJar = true;
            }
        }

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

    private boolean handleJarFileInJar(ZipInputStream jarInputInJar, String outerJarName, String innerJarName) {
        Set<String> packagePrefix4JarInTarJar = tarGzRecorder.getPackagePrefix4JarInTarJar(outerJarName, innerJarName);
        // 根据jar包中的包名前缀判断jar包是否需要解压
        if (!checkUnpackJarByPackagePrefix(packagePrefix4JarInTarJar, innerJarName)) {
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
        if (!JavaCGFileUtil.isDirectoryExists(file.getParent(), true)) {
            logger.error("创建文件所在目录失败 {}", file.getParent());
            throw new JavaCGRuntimeException("创建文件所在目录失败 " + file.getParent());
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
     * 根据jar包中的包名前缀判断jar包是否需要解压
     *
     * @param jarPackagePrefixSet jar包中的包名前缀
     * @param jarName             jar包名称
     * @return
     */
    private boolean checkUnpackJarByPackagePrefix(Set<String> jarPackagePrefixSet, String jarName) {
        String jarOnlyFileName = getFileNameFromPath(jarName);
        List<String> jarPackagePrefixList = new ArrayList<>(jarPackagePrefixSet);
        if (jarPackagePrefixList.isEmpty()) {
            jarPackagePrefixList.add("");
        }
        try {
            for (String jarPackagePrefix : jarPackagePrefixList) {
                JavaCGFileUtil.write2FileWithTab(jarPackagePrefixWriter, jarOnlyFileName, jarPackagePrefix, jarName);
            }
        } catch (IOException e) {
            logger.error("写文件失败 {} ", jarPackagePrefixFilePath, e);
        }

        if (unpackAllPackage) {
            logger.info("所有的jar包都[需要]解压 {} {}", tarGzFileName, jarName);
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
