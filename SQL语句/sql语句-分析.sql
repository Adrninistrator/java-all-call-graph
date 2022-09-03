-- ����������
select full_name, simple_name from class_name_xxx where simple_name in
(
select simple_name from class_name_xxx group by simple_name having count(simple_name) > 1
)

-- ���������෽������
select * from method_call_xxx where caller_class_name like '%.%'
select * from method_call_xxx where callee_class_name like '%.%'

-- ������ķ���������
select max(length(caller_method)),max(length(callee_method)) from method_call_xxx

-- �����ض�����
select * from method_call_xxx where caller_full_class_name like '%.dto.%'
select * from method_call_xxx where callee_full_class_name like '%.entity.%'

-- �����ض��ؼ��ַ���
select * from method_call_xxx where caller_method like '%<init%'
select * from method_call_xxx where callee_method like '%<init%'

-- ����ȥ����������
select distinct(caller_full_class_name) from method_call_xxx order by caller_full_class_name
select distinct(callee_full_class_name) from method_call_xxx order by callee_full_class_name

-- ���ұ����ô������ķ���
select r.method, count(r.method) as cm from
(
select concat(callee_full_class_name,':',callee_method_name) as method from method_call_xxx
) as r group by method order by cm desc

-- ���ұ����ô���������
select callee_full_class_name, count(callee_full_class_name) as cc from method_call_xxx group by callee_full_class_name order by cc desc

-- ���Ұ����ؼ��ֵ�ȥ����������
select distinct(caller_full_class_name) from method_call_xxx where caller_full_class_name like '%.proto.%'
select distinct(callee_full_class_name) from method_call_xxx where callee_full_class_name like '%.proto.%'

-- ����ȥ�ط�����
select caller_method_name,count(caller_method_name) as cc from method_call_xxx group by caller_method_name order by cc desc, caller_method_name
select callee_method_name,count(callee_method_name) as cc from method_call_xxx group by callee_method_name order by cc desc, callee_method_name

-- �����ض�������
select * from method_call_xxx where caller_method_name = ''
select * from method_call_xxx where callee_method_name = ''

-- �����ض������÷���������Ӧ����
select distinct(callee_full_class_name), callee_method_name from method_call_xxx where callee_method_name in ('toDenseJson','')

-- ���Ҷ�Ӧ���ʵ���������Ľӿڻ��࣬���������������ӿڻ��෽���б������������ã���H2���ݿ��﷨
SELECT DISTINCT r2.cc,  r1."caller_full_method"
FROM "jacg"."method_call_xxx" r1
JOIN 
(
	SELECT COUNT(eemh) AS cc, ermh FROM
	(
		SELECT DISTINCT a."callee_method_hash" AS eemh, a."caller_method_hash" AS ermh
		FROM "jacg"."method_call_xxx" a, "jacg"."method_call_xxx" b
		WHERE a."call_type" IN ('ITF','SCC')
		AND a."caller_method_hash" = b."callee_method_hash"
		AND b."call_type" NOT IN ('ITF','SCC','CCS')
	)
	GROUP BY ermh
	HAVING cc > 1
) r2
ON r1."caller_method_hash" = r2.ermh
ORDER BY r2.cc DESC

-- ���Ҷ�Ӧ���ʵ���������Ľӿڻ��࣬���Ҽ��������ӿڻ��෽���б������������ã���H2���ݿ��﷨
SELECT COUNT(eecn) AS cc, ercn FROM
(
	SELECT DISTINCT a."callee_class_name" AS eecn, a."caller_class_name" AS ercn
	FROM "jacg"."method_call_xxx" a, "jacg"."method_call_xxx" b
	WHERE a."call_type" IN ('ITF','SCC')
	AND a."caller_class_name" = b."callee_class_name"
	AND b."call_type" NOT IN ('ITF','SCC','CCS')
)
GROUP BY ercn
HAVING cc > 1
ORDER BY cc DESC
