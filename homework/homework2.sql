create database flightsSMW;

use flightsSMW;

create table jan08 (RowNum char(8), Year int(4), Month int(2),
DayOfMonth int(2), DayOfWeek int(1), DepTime int(4),
CRSDepTime int(4), ArrTime int(4), CRSArrTime int(4),
UniqueCarrier varchar(20), FlightNum int(8),
TailNum varchar(20), ActualElapsedTime int(5), CRSElapsedTime int(5),
AirTime int(5), ArrDelay int(4), DepDelay int(4), Origin char(10),
Dest char(10), Distance integer(6), TaxiIn int(3), TaxiOut int(3),
Cancelled int(2), CancellationCode varchar(20), Diverted varchar(20),
CarrierDelay varchar(20), WeatherDelay varchar(20), NASDelay varchar(20),
SecurityDelay varchar(20), LateAircraftDelay varchar(20));

create table planeData (tailNum varchar(10) Not NULL, type varchar(255), manufacturer varchar(255),
issueDate varchar(255), model varchar(255), status varchar(255),
aircraftType varchar(255), engineType varchar(255), year varchar(255));

create table carriers (Code varchar(255), description varchar(255));

create table airports (iata varchar(255), airport varchar(255), city varchar(255), state varchar(30),
country varchar(30), lattitude float(15), longitude float(15));



load data local
infile "/Users/Marc/Downloads/FlightData/jan08.csv"
into table jan08
fields terminated by ","
ignore 1 lines;

load data local
infile "/Users/Marc/Downloads/FlightData/plane-data.csv"
into table planeData
fields terminated by ","
ignore 1 lines;

load data local
infile "/Users/Marc/Downloads/FlightData/carriers.csv"
into table carriers
fields terminated by ","
ignore 1 lines;

load data local
infile "/Users/Marc/Downloads/FlightData/airports.csv"
into table airports
fields terminated by ","
ignore 1 lines;

