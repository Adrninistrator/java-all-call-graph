CREATE TABLE if not exists jacg_spring_bean_{appName} (
  record_id int NOT NULL COMMENT '记录id',
  spring_bean_name varchar(500) NOT NULL COMMENT 'Spring Bean的名称',
  seq int NOT NULL COMMENT '序号，从0开始，大于0代表有多种可能',
  class_name varchar(500) NOT NULL COMMENT '完整类名',
  PRIMARY KEY (record_id),
  INDEX inx_spb_sbn_{appName}(spring_bean_name(255)),
  INDEX inx_spb_cn_{appName}(class_name(255))
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='Spring Bean信息表';