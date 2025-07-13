CREATE TABLE if not exists jacg_spring_bean_{appName} (
  record_id int NOT NULL COMMENT '记录id，从1开始',
  spring_bean_name varchar(255) NOT NULL COMMENT 'Spring Bean的名称',
  seq int NOT NULL COMMENT '序号，从0开始，大于0代表有多种可能',
  simple_class_name varchar(255) NOT NULL COMMENT '唯一类名',
  class_name varchar(255) NOT NULL COMMENT '完整类名',
  bean_type varchar(2) NOT NULL COMMENT 'Spring Bean的定义方式，j: 在Java代码中定义，x: 在XML文件中定义',
  annotation_class_name varchar(255) NOT NULL COMMENT '在Java代码中定义时对应的注解类名',
  define_class_name_xml_path varchar(255) NOT NULL COMMENT '在Java代码中定义时所在的类名，或在XML中定义时对应的文件路径',
  PRIMARY KEY (record_id),
  INDEX idx_spb_sbn_{appName}(spring_bean_name),
  INDEX idx_spb_scn_{appName}(simple_class_name)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='Spring Bean信息表';