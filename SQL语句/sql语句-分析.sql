-- 查找重名类
select full_name, simple_name from class_name_wfts_fcts_adm where simple_name in
(
select simple_name from class_name_wfts_fcts_adm group by simple_name having count(simple_name) > 1
)

-- 查找重名类方法调用
select * from method_call_xxx where caller_class_name like '%.%'
select * from method_call_xxx where callee_class_name like '%.%'

-- 查找最长的方法名长度
select max(length(caller_method)),max(length(callee_method)) from method_call_xxx

-- 查找特定类名
select * from method_call_xxx where caller_full_class_name like '%.dto.%'
select * from method_call_xxx where callee_full_class_name like '%.entity.%'

-- 查找特定关键字方法
select * from method_call_xxx where caller_method like '%<init%'
select * from method_call_xxx where callee_method like '%<init%'

-- 查找去重完整类名
select distinct(caller_full_class_name) from method_call_xxx order by caller_full_class_name
select distinct(callee_full_class_name) from method_call_xxx order by callee_full_class_name

-- 查找被调用次数最多的方法
select r.method, count(r.method) as cm from
(
select concat(callee_full_class_name,':',callee_method_name) as method from method_call_xxx
) as r group by method order by cm desc

-- 查找被调用次数最多的类
select callee_full_class_name, count(callee_full_class_name) as cc from method_call_xxx group by callee_full_class_name order by cc desc

-- 查找包含关键字的去重完整类名
select distinct(caller_full_class_name) from method_call_xxx where caller_full_class_name like '%.proto.%'
select distinct(callee_full_class_name) from method_call_xxx where callee_full_class_name like '%.proto.%'

-- 查找去重方法名
select caller_method_name,count(caller_method_name) as cc from method_call_xxx group by caller_method_name order by cc desc, caller_method_name
select callee_method_name,count(callee_method_name) as cc from method_call_xxx group by callee_method_name order by cc desc, callee_method_name

-- 查找特定方法名
select * from method_call_xxx where caller_method_name = ''
select * from method_call_xxx where callee_method_name = ''

-- 查找特定被调用方法名及对应的类
select distinct(callee_full_class_name), callee_method_name from method_call_xxx where callee_method_name in ('toDenseJson','')

-- 查找对应多个实现类或子类的接口或父类
SELECT COUNT(combine) AS cc , ercn FROM
(
SELECT DISTINCT CONCAT("caller_class_name", ':', "callee_class_name") AS combine, "caller_class_name" AS ercn
FROM "jacg"."method_call_xxx"
WHERE "call_type" IN ('ITF','SCC')
)
GROUP BY ercn
HAVING cc > 1
ORDER BY cc DESC;
