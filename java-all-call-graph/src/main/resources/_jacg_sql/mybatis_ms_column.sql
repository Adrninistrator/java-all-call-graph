CREATE TABLE if not exists jacg_mybatis_ms_column_{appName} (
  record_id int NOT NULL COMMENT '记录id，从1开始',
  result_map_id varchar(100) NOT NULL COMMENT 'XML的resultMap ID',
  entity_simple_class_name varchar(255) NOT NULL COMMENT 'MyBatis Entity类唯一类名',
  entity_field_name varchar(200) NOT NULL COMMENT 'Entity类字段名',
  column_name varchar(200) NOT NULL COMMENT '数据库字段名（MyBatis XML中可能使用函数，长度需要长一些）',
  column_type varchar(50) NOT NULL COMMENT '数据库字段类型',
  entity_class_name varchar(255) NOT NULL COMMENT 'MyBatis Entity类完整类名',
  xml_file_name varchar(255) NOT NULL COMMENT 'MyBatis XML文件名',
  xml_file_path varchar(500) NOT NULL COMMENT 'MyBatis XML文件路径',
  PRIMARY KEY (record_id),
  INDEX idx_mmc_escf_{appName}(entity_simple_class_name, entity_field_name),
  INDEX idx_mmc_xfn_{appName}(xml_file_name)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='MyBatis的Entity与数据库字段名信息（使用MySQL）';