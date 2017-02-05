use sakila;

-- 1 Write an SQL query that will return the number of cities within each country along with the country name.

Select country.country, count(city.city) as counted
	from city
    join country on city.country_id=country.country_id
    group by country;

-- 2 Edit the 1st code so that the result is ordered by the count of cities in descending order.

Select country.country, count(city.city) as counted
	from city
    join country on city.country_id=country.country_id
    group by country
    order by counted desc;
    
-- 3 Edit the code in part 2 to only return counts of 3 or more.

Select a.* from 
	(Select country.country, count(city.city) counted
		from city
		join country on city.country_id=country.country_id
		group by country
		order by counted desc) a
	where counted >= 3;
    
-- 4 Use the customer table to find all the customers whose first name matches another customers last name 
	-- (e.g. "Scott" could be the first name of one or more customer and the last name of others).

Select last_name from customer inner join
	(SELECT first_name from customer) a on last_name = a.first_name;

-- 5 Use the payment, staff, and store tables to calculate the total and average payement by store.

select store.store_id, sum(p.amount), avg(p.amount)
	from payment p join staff on p.staff_id = staff.staff_id
		join store on store.store_id = staff.staff_id
	group by store_id;


-- 6 Create a View of the previous results

create view Problem6 as (
select store.store_id, sum(p.amount), avg(p.amount)
	from payment p join staff on p.staff_id = staff.staff_id
		join store on store.store_id = staff.staff_id
	group by store_id);

-- 7 Write the SQL that will return the rows from the payment table where the amount of the 
-- payment is greater than the average for the corresponding store.
    
select * from payment p
	join (select staff_id, avg(amount) average from payment group by staff_id) a on p.staff_id = a.staff_id
    where p.amount > a.average
	order by p.staff_id;
    
    
    
