/*
  File:   Hometask0.sql
  Course: DataBase practice (471 group, term 7)
  Author: Aleksei Semin (alllex.semin@gmail.com)
  Date:   09.2015 (c)
  Misc:   Script works in default workspace of apex.oracle.com 
*/

-- Task 2

-- What are the jobs?
select distinct job from emp;

-- Task 3

-- Employees per department.
select (select dname from dept where dept.deptno=emp.deptno
       ) dname, 
       count(*) 
from emp 
group by deptno;

-- Average salary per job.
select job, round(avg(sal)) from emp group by job;

-- Task 4 

-- Min, max and summed salary per job.
select job, 
       min(sal+nvl(comm,0)) min, 
       max(sal+nvl(comm,0)) max, 
       sum(sal+nvl(comm,0)) sum 
from emp 
group by job;

-- Task 5

-- Pairs of managers.
select emp1.ename as n1, emp2.ename as n2 
from emp emp1, emp emp2 
where emp1.job='MANAGER'
and emp2.job='MANAGER'
and emp1.ename>emp2.ename 
order by 1,2;

-- Task 6

-- Managers and workers
select * 
from (select ename, 
             (select count(*) from emp where mgr = emp1.empno
             ) count_emp 
      from emp emp1) 
where count_emp <> 0;

-- Task 7

-- Bosses, summed salary and count.
select (select emp1.ename 
        from emp emp1 
        where empp.mgr = emp1.empno
       ) mgrName, 
       sumSal, 
       Num 
from (select mgr, 
             sum(sal) sumSal, 
             count(*) Num 
      from emp 
      group by mgr) empp;















