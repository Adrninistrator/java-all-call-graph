package test.callgraph.mybatis.entity;

import java.util.Date;

public class TestTableGjcusd {
    private String idC;

    private String flag1C;

    private String flag2C;

    private Date createTime;

    private Date updateTime;

    public String getIdC() {
        return idC;
    }

    public void setIdC(String idC) {
        this.idC = idC == null ? null : idC.trim();
    }

    public String getFlag1C() {
        return flag1C;
    }

    public void setFlag1C(String flag1C) {
        this.flag1C = flag1C == null ? null : flag1C.trim();
    }

    public String getFlag2C() {
        return flag2C;
    }

    public void setFlag2C(String flag2C) {
        this.flag2C = flag2C == null ? null : flag2C.trim();
    }

    public Date getCreateTime() {
        return createTime;
    }

    public void setCreateTime(Date createTime) {
        this.createTime = createTime;
    }

    public Date getUpdateTime() {
        return updateTime;
    }

    public void setUpdateTime(Date updateTime) {
        this.updateTime = updateTime;
    }
}