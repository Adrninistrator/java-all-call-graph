CREATE TABLE if not exists class_name_{appName} (
  full_name varchar(200) NOT NULL COMMENT '完整类名',
  simple_name varchar(100) NOT NULL COMMENT '唯一类名',
  PRIMARY KEY (full_name),
  INDEX idx_cn_simple_name(simple_name)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='类名信息表';