CREATE TABLE if not exists jacg_mybatis_ms_write_table_{appName} (
  record_id int NOT NULL COMMENT '记录id，从1开始',
  mapper_simple_class_name varchar(255) NOT NULL COMMENT 'MyBatis Mapper唯一类名',
  mapper_method_name varchar(200) NOT NULL COMMENT 'MyBatis Mapper方法名',
  sql_statement varchar(15) NOT NULL COMMENT '写操作sql语句类型',
  table_name varchar(200) NOT NULL COMMENT '数据库表名（MyBatis XML中可能使用函数，长度需要长一些）',
  mapper_class_name varchar(255) NOT NULL COMMENT 'MyBatis Mapper完整类名',
  xml_file_name varchar(255) NOT NULL COMMENT 'MyBatis XML文件名',
  xml_file_path varchar(500) NOT NULL COMMENT 'MyBatis XML文件路径',
  PRIMARY KEY (record_id),
  INDEX idx_mmwt_scm_{appName}(mapper_simple_class_name, mapper_method_name),
  INDEX idx_mmwt_xfn_{appName}(xml_file_name)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='MyBatis Mapper方法写的数据库表信息（使用MySQL）';