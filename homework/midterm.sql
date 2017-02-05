use flightsSMW;

create table midterm_time (
id int primary key auto_increment,
test_date datetime);

create table midterm_score (
id int primary key auto_increment,
name varchar(8),
score int);


create trigger after_midterm_score_insert
	after insert on midterm_score
    for each row
	insert into midterm_time (test_date)
    values(now());

insert into midterm_score (name, score)
values("allen",37),("cody",83),("marc",99),("greg",2),("mike",73.5);

drop trigger after_midtermScore_insert;
drop tables midterm_time, midterm_score;


DELIMITER $$
CREATE FUNCTION flightsSMW.checkTimestamp (x datetime, y datetime)
RETURNS bool
begin
	declare timeCorrect bool;
    if x < y then
		set timeCorrect = TRUE;
	else set timeCorrect = FALSE;
	end if;	
RETURN timeCorrect;
end$$
DELIMITER ;

create table midterm_retake (
id int primary key auto_increment,
test_date datetime);

insert into midterm_retake (test_date)
	values (now()),(now()),(now()),(now()),(now()-1000000),(now()),(now()),(now()),(NULL),(now());

drop tables midterm_retake;


select s.name, s.score, checkTimestamp(a.test_date,b.test_date) as retake
	from midterm_score s
    join midterm_time a on s.id = a.id
    join midterm_retake b on s.id = b.id;

drop FUNCTION flightsSMW.checkTimestamp;





