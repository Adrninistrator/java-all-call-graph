package com.adrninistrator.jacg.dto.writedb;

import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData;

/**
 * @author adrninistrator
 * @date 2022/11/17
 * @description: 用于写入数据库的数据，Spring Controller信息
 */
public class WriteDbData4SpringController implements BaseWriteDbData {

    private int recordId;
    private String methodHash;
    private int seq;
    private String showUri;
    private String classPath;
    private String methodPath;
    private String annotationName;
    private String simpleClassName;
    private String fullMethod;
    private String returnType;
    private int maybeFileUpload;
    private int maybeFileDownload;

    public int getRecordId() {
        return recordId;
    }

    public void setRecordId(int recordId) {
        this.recordId = recordId;
    }

    public String getMethodHash() {
        return methodHash;
    }

    public void setMethodHash(String methodHash) {
        this.methodHash = methodHash;
    }

    public int getSeq() {
        return seq;
    }

    public void setSeq(int seq) {
        this.seq = seq;
    }

    public String getShowUri() {
        return showUri;
    }

    public void setShowUri(String showUri) {
        this.showUri = showUri;
    }

    public String getClassPath() {
        return classPath;
    }

    public void setClassPath(String classPath) {
        this.classPath = classPath;
    }

    public String getMethodPath() {
        return methodPath;
    }

    public void setMethodPath(String methodPath) {
        this.methodPath = methodPath;
    }

    public String getAnnotationName() {
        return annotationName;
    }

    public void setAnnotationName(String annotationName) {
        this.annotationName = annotationName;
    }

    public String getSimpleClassName() {
        return simpleClassName;
    }

    public void setSimpleClassName(String simpleClassName) {
        this.simpleClassName = simpleClassName;
    }

    public String getFullMethod() {
        return fullMethod;
    }

    public void setFullMethod(String fullMethod) {
        this.fullMethod = fullMethod;
    }

    public String getReturnType() {
        return returnType;
    }

    public void setReturnType(String returnType) {
        this.returnType = returnType;
    }

    public int getMaybeFileUpload() {
        return maybeFileUpload;
    }

    public void setMaybeFileUpload(int maybeFileUpload) {
        this.maybeFileUpload = maybeFileUpload;
    }

    public int getMaybeFileDownload() {
        return maybeFileDownload;
    }

    public void setMaybeFileDownload(int maybeFileDownload) {
        this.maybeFileDownload = maybeFileDownload;
    }

    //
    @Override
    public String toString() {
        return "WriteDbData4SpringController{" +
                "methodHash='" + methodHash + '\'' +
                ", seq=" + seq +
                ", showUri='" + showUri + '\'' +
                ", classPath='" + classPath + '\'' +
                ", methodPath='" + methodPath + '\'' +
                ", simpleClassName='" + simpleClassName + '\'' +
                ", fullMethod='" + fullMethod + '\'' +
                '}';
    }
}
