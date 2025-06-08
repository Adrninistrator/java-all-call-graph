CREATE TABLE if not exists jacg_spring_task_{appName} (
  record_id int NOT NULL COMMENT '记录id，从1开始',
  method_hash varchar(32) NOT NULL COMMENT '方法hash+字节数',
  spring_bean_name varchar(255) NOT NULL COMMENT 'Spring Bean的名称',
  class_name varchar(255) NOT NULL COMMENT '完整类名',
  method_name varchar(200) NOT NULL COMMENT '方法名',
  type varchar(10) NOT NULL COMMENT '类型，XML: 在XML文件中定义，annotation: 通过注解定义',
  full_method text NOT NULL COMMENT '完整方法（类名+方法名+参数）',
  return_type varchar(255) NOT NULL COMMENT '方法返回类型，包含数组标志',
  PRIMARY KEY (record_id),
  INDEX idx_spt_mh_{appName}(method_hash),
  INDEX idx_spt_sbn_{appName}(spring_bean_name),
  INDEX idx_spt_cm_{appName}(class_name, method_name)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='Spring定时任务信息表';