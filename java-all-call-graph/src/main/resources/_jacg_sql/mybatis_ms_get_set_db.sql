CREATE TABLE if not exists jacg_mybatis_ms_get_set_db_{appName} (
  record_id int NOT NULL COMMENT '记录id，从1开始',
  fld_relationship_id int NOT NULL COMMENT '通过get/set方法关联的字段关系id，从1开始',
  get_or_set varchar(3) NOT NULL COMMENT '对应get方法还是set方法',
  get_method_call_id int NOT NULL COMMENT 'get方法对应的方法调用ID，从1开始',
  set_method_call_id int NOT NULL COMMENT 'set方法对应的方法调用ID，从1开始',
  db_operate varchar(20) NOT NULL COMMENT '数据库操作，包含sql语句，除select、insert、update、delete外，后面可能加上@set、@where',
  table_name varchar(200) NOT NULL COMMENT '数据库表名（MyBatis XML中可能使用函数，长度需要长一些）',
  column_name varchar(200) NOT NULL COMMENT '数据库字段名（MyBatis XML中可能使用函数，长度需要长一些）',
  column_relate_desc varchar(10) NOT NULL COMMENT 'MyBatis字段与Java代码字段关联方式描述，参考 MyBatisColumnRelateDescEnum 枚举类',
  PRIMARY KEY (record_id),
  INDEX idx_mmgsd_{appName}(fld_relationship_id),
  INDEX idx_gmci_{appName}(get_method_call_id),
  INDEX idx_smci_{appName}(set_method_call_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='使用MyBatis时get/set方法所关联的数据库信息（使用MySQL）';