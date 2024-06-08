CREATE TABLE if not exists jacg_field_info_{appName} (
  record_id int NOT NULL COMMENT '记录id，从1开始',
  simple_class_name varchar(255) NOT NULL COMMENT '唯一类名',
  field_name varchar(200) NOT NULL COMMENT '字段名称',
  field_type varchar(255) NOT NULL COMMENT '字段类型',
  modifiers varchar(10) NOT NULL COMMENT '字段修饰符',
  primitive_type tinyint NOT NULL COMMENT '基本类型，1:是，0:否',
  static_flag tinyint NOT NULL COMMENT 'static标志，1:是，0:否',
  final_flag tinyint NOT NULL COMMENT 'final标志，1:是，0:否',
  class_name varchar(255) NOT NULL COMMENT '完整类名',
  PRIMARY KEY (record_id),
  INDEX idx_fi_scnft_{appName}(simple_class_name, field_type)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='字段信息表';