package com.adrninistrator.jacg.unpacker.targz;

import com.adrninistrator.jacg.unpacker.common.enums.JarFileLocationEnum;
import com.adrninistrator.jacg.unpacker.dto.JarPackagePrefixInfo;
import net.lingala.zip4j.io.inputstream.ZipInputStream;
import net.lingala.zip4j.model.LocalFileHeader;
import org.apache.tools.tar.TarEntry;
import org.apache.tools.tar.TarInputStream;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2024/1/18
 * @description: 用于记录.tar.gz中的jar包中的包名前缀信息
 */
public class TarGzRecorder extends BaseTarGzUnpacker {

    private static final Logger logger = LoggerFactory.getLogger(TarGzRecorder.class);

    /**
     * tar.gz中的.jar中的包名前缀
     * key      jar包路径
     * value    jar包中的包名前缀列表（a/b/c/d/）
     */
    private final Map<String, Set<String>> tarJarPackagePrefixMap = new HashMap<>();

    /**
     * tar.gz中的.war中的包名前缀
     * key      war包路径
     * value    war包中的包名前缀列表（a/b/c/d/）
     */
    private final Map<String, Set<String>> tarWarPackagePrefixMap = new HashMap<>();

    /**
     * tar.gz中的.war中的.jar中的包名前缀
     * key      war包路径
     * value    同上
     */
    private final Map<String, Map<String, Set<String>>> tarWarJarPackagePrefixMap = new HashMap<>();

    /**
     * tar.gz中的.jar中的.jar中的包名前缀（Spring Boot使用Maven打包时BOOT-INF/lib中的jar包）
     * key      jar包路径
     * value    同上
     */
    private final Map<String, Map<String, Set<String>>> tarJarJarPackagePrefixMap = new HashMap<>();

    public TarGzRecorder(String tarGzFilePath) {
        super(tarGzFilePath);
    }

    @Override
    protected boolean handleWarFileInTar(ZipInputStream warInputInTar, TarEntry tarEntry) {
        String warNameOfTarEntry = tarEntry.getName();
        // 记录tar.gz中的.war中的.class文件包名前缀
        Set<String> warPackagePrefixSet = new HashSet<>();
        tarWarPackagePrefixMap.put(warNameOfTarEntry, warPackagePrefixSet);
        try {
            LocalFileHeader warEntry;
            while ((warEntry = warInputInTar.getNextEntry()) != null) {
                if (warEntry.isDirectory()) {
                    continue;
                }

                String warEntryName = warEntry.getFileName();
                if (checkClassFileType(warEntryName)) {
                    // war包中的class文件都进行处理，不忽略（war包一般都是子系统的代码）
                    // 将包名前缀记录到Set中
                    recordClassFilePackagePrefix(warPackagePrefixSet, warEntryName);
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
        return true;
    }

    @Override
    protected boolean handleJarFileInTarWar(ZipInputStream jarInputInTarWar, String warNameOfTarEntry, String jarNameOfTarWarEntry) {
        try {
            // 记录tar.gz中的.jar中的.class文件包名前缀
            Set<String> jarPackagePrefixSet4JarInTar = null;
            // 记录tar.gz中的.war中的.jar中的.class文件包名前缀
            Set<String> jarPackagePrefixSet4JarInTarWar = null;
            if (warNameOfTarEntry == null) {
                jarPackagePrefixSet4JarInTar = new HashSet<>();
                tarJarPackagePrefixMap.put(jarNameOfTarWarEntry, jarPackagePrefixSet4JarInTar);
            } else {
                Map<String, Set<String>> warJarPackagePrefixMap = tarWarJarPackagePrefixMap.computeIfAbsent(warNameOfTarEntry, k -> new HashMap<>());
                jarPackagePrefixSet4JarInTarWar = new HashSet<>();
                warJarPackagePrefixMap.put(jarNameOfTarWarEntry, jarPackagePrefixSet4JarInTarWar);
            }

            LocalFileHeader jarEntry;
            while ((jarEntry = jarInputInTarWar.getNextEntry()) != null) {
                if (jarEntry.isDirectory()) {
                    continue;
                }

                String jarEntryName = jarEntry.getFileName();
                if (checkClassFileType(jarEntryName)) {
                    if (warNameOfTarEntry == null) {
                        // 记录tar.gz中的.jar中的.class文件包名前缀
                        recordClassFilePackagePrefix(jarPackagePrefixSet4JarInTar, jarEntryName);
                    } else {
                        // 记录tar.gz中的.war中的.jar中的.class文件包名前缀
                        recordClassFilePackagePrefix(jarPackagePrefixSet4JarInTarWar, jarEntryName);
                    }
                } else if (checkJarFileType(jarEntryName)) {
                    // spring-boot maven生成的jar包中会存在BOOT-INF/lib/目录，该目录中有其他jar包
                    if (!handleJarFileInJar(new ZipInputStream(jarInputInTarWar), jarNameOfTarWarEntry, jarEntryName)) {
                        return false;
                    }
                }
            }
            return true;
        } catch (Exception e) {
            logger.error("error {} {} {}", tarGzFileName, warNameOfTarEntry, jarNameOfTarWarEntry, e);
            return false;
        }
    }

    // 处理jar包中的jar包
    private boolean handleJarFileInJar(ZipInputStream jarInputInJar, String outerJarName, String innerJarName) {
        try {
            Map<String, Set<String>> jarJarPackagePrefixMap = tarJarJarPackagePrefixMap.computeIfAbsent(outerJarName, k -> new HashMap<>());
            // 记录tar.gz中的.jar中的.jar中的.class文件包名前缀
            Set<String> jarPackagePrefixSet = new HashSet<>();
            jarJarPackagePrefixMap.put(innerJarName, jarPackagePrefixSet);
            LocalFileHeader jarEntry;
            while ((jarEntry = jarInputInJar.getNextEntry()) != null) {
                if (jarEntry.isDirectory()) {
                    continue;
                }

                String jarEntryName = jarEntry.getFileName();
                if (checkClassFileType(jarEntryName)) {
                    // 将包名前缀记录到Set中
                    recordClassFilePackagePrefix(jarPackagePrefixSet, jarEntryName);
                }
            }
            return true;
        } catch (Exception e) {
            logger.error("error {} {} {} ", tarGzFileName, outerJarName, innerJarName, e);
            return false;
        }
    }

    /**
     * 获取.tar.gz中的.jar中的包名前缀
     *
     * @param jarNameOfTarEntry
     * @return
     */
    public Set<String> getPackagePrefix4JarInTar(String jarNameOfTarEntry) {
        return tarJarPackagePrefixMap.get(jarNameOfTarEntry);
    }

    /**
     * 获取.tar.gz中的.war中的.jar中的包名前缀
     *
     * @param warNameOfTarEntry
     * @param jarNameOfWarEntry
     * @return
     */
    public Set<String> getPackagePrefix4JarInTarWar(String warNameOfTarEntry, String jarNameOfWarEntry) {
        return tarWarJarPackagePrefixMap.get(warNameOfTarEntry).get(jarNameOfWarEntry);
    }

    /**
     * 获取
     *
     * @param outerJarName
     * @param innerJarName
     * @return
     */
    public Set<String> getPackagePrefix4JarInTarJar(String outerJarName, String innerJarName) {
        return tarJarJarPackagePrefixMap.get(outerJarName).get(innerJarName);
    }

    /**
     * 生成jar包中的包名前缀信息
     *
     * @return
     */
    public List<JarPackagePrefixInfo> genJarPackagePrefixInfoList() {
        List<JarPackagePrefixInfo> jarPackagePrefixInfoList = new ArrayList<>();
        // 处理.tar.gz中的.jar
        addJarPackagePrefixInfo1(jarPackagePrefixInfoList, tarJarPackagePrefixMap, JarFileLocationEnum.JFLE_TAR_JAR, null);
        // 处理.tar.gz中的.war
        addJarPackagePrefixInfo1(jarPackagePrefixInfoList, tarWarPackagePrefixMap, JarFileLocationEnum.JFLE_TAR_WAR, null);
        // 处理.tar.gz中的.war中的.jar
        addJarPackagePrefixInfo2(jarPackagePrefixInfoList, tarWarJarPackagePrefixMap, JarFileLocationEnum.JFLE_TAR_WAR_JAR);
        // 处理.tar.gz中的.jar中的.jar
        addJarPackagePrefixInfo2(jarPackagePrefixInfoList, tarJarJarPackagePrefixMap, JarFileLocationEnum.JFLE_TAR_JAR_JAR);
        return jarPackagePrefixInfoList;
    }

    // 记录jar包中的包名前缀信息，1层结构时使用
    private void addJarPackagePrefixInfo1(List<JarPackagePrefixInfo> jarPackagePrefixInfoList, Map<String, Set<String>> packagePrefixMap1,
                                          JarFileLocationEnum jarFileLocationEnum, String warOrOuterJarFileName) {
        List<String> jarWarFileNameList = new ArrayList<>(packagePrefixMap1.keySet());
        Collections.sort(jarWarFileNameList);
        for (String jarWarFileName : jarWarFileNameList) {
            List<String> packagePrefixList = new ArrayList<>(packagePrefixMap1.get(jarWarFileName));
            Collections.sort(packagePrefixList);
            for (String packagePrefix : packagePrefixList) {
                JarPackagePrefixInfo jarPackagePrefixInfo = new JarPackagePrefixInfo();
                jarPackagePrefixInfo.setTarFileName(tarGzFileName);
                jarPackagePrefixInfo.setJarFileLocationEnum(jarFileLocationEnum);
                switch (jarFileLocationEnum) {
                    case JFLE_TAR_JAR:
                        setJarFileName(jarPackagePrefixInfo, jarWarFileName);
                        break;
                    case JFLE_TAR_WAR:
                        setWarOrOuterJarFileName(jarPackagePrefixInfo, jarWarFileName);
                        break;
                    case JFLE_TAR_WAR_JAR:
                    case JFLE_TAR_JAR_JAR:
                        setWarOrOuterJarFileName(jarPackagePrefixInfo, warOrOuterJarFileName);
                        setJarFileName(jarPackagePrefixInfo, jarWarFileName);
                        break;
                }

                jarPackagePrefixInfo.setPackagePrefix(packagePrefix);
                jarPackagePrefixInfoList.add(jarPackagePrefixInfo);
            }
        }
    }

    // 记录jar包中的包名前缀信息，2层结构时使用
    private void addJarPackagePrefixInfo2(List<JarPackagePrefixInfo> jarPackagePrefixInfoList, Map<String, Map<String, Set<String>>> packagePrefixMap2,
                                          JarFileLocationEnum jarFileLocationEnum) {
        List<String> warOrOuterJarFileNameList = new ArrayList<>(packagePrefixMap2.keySet());
        Collections.sort(warOrOuterJarFileNameList);
        for (String warOrOuterJarFileName : warOrOuterJarFileNameList) {
            Map<String, Set<String>> packagePrefixMap1 = packagePrefixMap2.get(warOrOuterJarFileName);
            addJarPackagePrefixInfo1(jarPackagePrefixInfoList, packagePrefixMap1, jarFileLocationEnum, warOrOuterJarFileName);
        }
    }

    // 设置.tar.gz中的.war或外层.jar文件名称
    private void setWarOrOuterJarFileName(JarPackagePrefixInfo jarPackagePrefixInfo, String warOrOuterJarFileName) {
        jarPackagePrefixInfo.setWarOrOuterJarFileName(warOrOuterJarFileName);
        if (!warOrOuterJarFileName.isEmpty()) {
            jarPackagePrefixInfo.setWarOrOuterJarFileOnlyName(getFileNameFromPath(warOrOuterJarFileName));
        }
    }

    // 设置.tar.gz中的.jar，或.tar.gz中的.war中的.jar，或.tar.gz中的.jar中的.jar文件名称
    private void setJarFileName(JarPackagePrefixInfo jarPackagePrefixInfo, String jarFileName) {
        jarPackagePrefixInfo.setJarFileName(jarFileName);
        if (!jarFileName.isEmpty()) {
            jarPackagePrefixInfo.setJarFileOnlyName(getFileNameFromPath(jarFileName));
        }
    }
}
