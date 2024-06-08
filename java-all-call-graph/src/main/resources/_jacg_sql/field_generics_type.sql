CREATE TABLE if not exists jacg_field_generics_type_{appName} (
  record_id int NOT NULL COMMENT '记录id，从1开始',
  simple_class_name varchar(255) NOT NULL COMMENT '唯一类名',
  field_name varchar(200) NOT NULL COMMENT '字段名',
  seq int NOT NULL COMMENT '字段集合中的泛型类型序号，从0开始',
  field_category varchar(5) NOT NULL COMMENT '字段集合中的泛型类型分类，J:JDK中的类型，C:自定义类型',
  simple_field_generics_type varchar(255) DEFAULT NULL COMMENT '字段集合中的泛型类型唯一类名',
  field_generics_type varchar(255) NOT NULL COMMENT '字段集合中的泛型类型',
  class_name varchar(255) NOT NULL COMMENT '完整类名',
  PRIMARY KEY (record_id),
  INDEX idx_fgt_cfs_{appName}(simple_class_name, field_name, seq),
  INDEX idx_fgt_fc_{appName}(field_category),
  INDEX idx_fgt_sfgt_{appName}(simple_field_generics_type)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='dto的非静态字段集合中涉及的泛型类型';