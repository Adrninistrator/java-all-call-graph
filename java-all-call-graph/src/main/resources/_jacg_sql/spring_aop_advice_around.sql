CREATE TABLE IF NOT EXISTS jacg_spring_aop_advice_around_{appName} (
  record_id int NOT NULL COMMENT '记录id，从1开始',
  advice_full_method text NOT NULL COMMENT 'advice的完整方法',
  advice_method_return_type varchar(255) NOT NULL COMMENT 'advice方法的返回类型',
  advice_method_hash varchar(32) NOT NULL COMMENT 'advice方法hash+字节数',
  proceed_call_id int NOT NULL COMMENT '调用ProceedingJoinPoint.proceed()方法调用序号，从1开始',
  PRIMARY KEY (record_id),
  INDEX idx_saaa_amh_{appName}(advice_method_hash)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='Spring AOP advice的Around信息';