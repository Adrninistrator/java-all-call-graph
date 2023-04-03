CREATE TABLE if not exists jacg_spring_task_{appName} (
  record_id int NOT NULL COMMENT '记录id',
  spring_bean_name varchar(500) NOT NULL COMMENT 'Spring Bean的名称',
  class_name varchar(500) NOT NULL COMMENT '完整类名',
  method_name varchar(300) NOT NULL COMMENT '方法名',
  PRIMARY KEY (record_id),
  INDEX inx_spt_sbn_{appName}(spring_bean_name(255)),
  INDEX inx_spt_cn_{appName}(class_name(255))
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='Spring定时任务信息表';