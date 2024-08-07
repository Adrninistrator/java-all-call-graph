CREATE TABLE if not exists jacg_inner_class_{appName} (
  inner_simple_class_name varchar(255) NOT NULL COMMENT '内部类唯一类名',
  inner_class_name varchar(255) NOT NULL COMMENT '内部类完整类名',
  outer_simple_class_name varchar(255) NOT NULL COMMENT '外部类唯一类名',
  outer_class_name varchar(255) NOT NULL COMMENT '外部类完整类名',
  anonymous_class tinyint NOT NULL COMMENT '是否为匿名内部类，1:是，0:否',
  PRIMARY KEY (inner_simple_class_name),
  INDEX idx_ic_socn_{appName}(outer_simple_class_name)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='内部类的信息表';