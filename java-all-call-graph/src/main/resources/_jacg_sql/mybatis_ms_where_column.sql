CREATE TABLE IF NOT EXISTS jacg_mybatis_ms_where_column_{appName} (
  record_id int NOT NULL COMMENT '记录id，从1开始',
  mapper_simple_class_name varchar(300) NOT NULL COMMENT 'MyBatis Mapper唯一类名',
  mapper_method_name varchar(200) NOT NULL COMMENT 'MyBatis Mapper方法名',
  table_name varchar(200) NOT NULL COMMENT '数据库表名（MyBatis XML中可能使用函数，长度需要长一些）',
  column_name varchar(200) NOT NULL COMMENT '数据库字段名（MyBatis XML中可能使用函数，长度需要长一些）',
  operation varchar(20) NOT NULL COMMENT '数据库字段进行比较的方式',
  param_obj_name varchar(200) NOT NULL COMMENT '数据库字段赋值的参数对象名称',
  param_name varchar(200) NOT NULL COMMENT '数据库字段赋值的参数名称，不包含参数对象名称',
  param_raw_name varchar(200) NOT NULL COMMENT '数据库字段赋值的参数原始名称，包含参数对象名称',
  param_type char(1) NOT NULL COMMENT '数据库字段用于比较的参数的使用方式，#/$',
  mapper_class_name varchar(300) NOT NULL COMMENT 'MyBatis Mapper完整类名',
  xml_file_name varchar(255) NOT NULL COMMENT 'MyBatis XML文件名',
  xml_file_path varchar(500) NOT NULL COMMENT 'MyBatis XML文件路径',
  PRIMARY KEY (record_id),
  INDEX idx_mmwc_mscm_{appName}(mapper_simple_class_name(255), mapper_method_name),
  INDEX idx_mmwc_tncn_{appName}(table_name, column_name),
  INDEX idx_mmwc_xfn_{appName}(xml_file_name)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='MyBatis的XML中where子句的字段信息（使用MySQL）';