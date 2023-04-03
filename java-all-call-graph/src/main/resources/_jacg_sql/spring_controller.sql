CREATE TABLE if not exists jacg_spring_controller_{appName} (
  method_hash varchar(30) NOT NULL COMMENT '方法hash+字节数',
  seq int NOT NULL COMMENT '序号，从0开始，大于0代表有多种可能',
  show_uri varchar(500) NOT NULL COMMENT '用于显示的URI',
  class_path varchar(300) NOT NULL COMMENT '类上的注解path属性原始值',
  method_path varchar(300) NOT NULL COMMENT '方法上的注解path属性原始值',
  annotation_name varchar(500) NOT NULL COMMENT '注解类名',
  simple_class_name varchar(500) NOT NULL COMMENT '唯一类名',
  full_method text NOT NULL COMMENT '完整方法（类名+方法名+参数）',
  PRIMARY KEY (method_hash, seq),
  INDEX inx_spc_su_{appName}(show_uri(255)),
  INDEX inx_spc_scn_{appName}(simple_class_name(255))
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='Spring Bean信息表';