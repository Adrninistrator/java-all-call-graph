CREATE TABLE IF NOT EXISTS jacg_spring_di_field{appName} (
  record_id int NOT NULL COMMENT '记录id，从1开始',
  class_name varchar(300) NOT NULL COMMENT '完整类名',
  simple_class_name varchar(300) NOT NULL COMMENT '唯一类名',
  field_type varchar(300) NOT NULL COMMENT '字段声明类型（接口类型）',
  simple_field_type varchar(300) NOT NULL COMMENT '唯一字段声明类型',
  field_name varchar(255) NOT NULL COMMENT '字段名',
  bean_type varchar(300) NOT NULL COMMENT 'Spring Bean实际类型',
  simple_bean_type varchar(300) NOT NULL COMMENT '唯一Bean类型',
  same_type int NOT NULL COMMENT '字段声明类型与Spring Bean实际注入类型是否相同，0:不同，1:相同',
  PRIMARY KEY (record_id),
  INDEX idx_sdi_scn_sft{appName}(simple_class_name(255), simple_field_type),
  INDEX idx_sdi_scn_sbt{appName}(simple_class_name(255), simple_bean_type)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='Spring依赖注入字段信息表';


