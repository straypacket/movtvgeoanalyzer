!#/usb/bin/ruby
###
# Typical command line:
# for I in /Volumes/IO-Data_1/app*/production.log.*; do echo $I ; ruby1.9 qori_log_extractor.rb $I ; done
###
require 'file/tail'
require 'mysql'
require 'ostruct'
require 'zlib'
require 'date'

CONFIG = OpenStruct.new
CONFIG.host = ENV['MYSQL_HOST'] || '127.0.0.1'
CONFIG.port = ENV['MYSQL_PORT'] || '3306'
CONFIG.user = ENV['MYSQL_USER'] || 'skillup'
CONFIG.pass = ENV['MYSQL_PASS'] || 'skillup'
CONFIG.sock = ENV['MYSQL_SOCK']
CONFIG.flag = ENV['MYSQL_FLAG']
CONFIG.database = ENV['MYSQL_DATABASE'] || 'qori_analyzer'
CONFIG.file = ARGV[0]

def init()
	db_setup()
	extract_loop()
end

def db_setup()
    @host = CONFIG.host
    @user = CONFIG.user
    @pass = CONFIG.pass
    @db   = CONFIG.database

    @port = CONFIG.port.to_i
    @sock = CONFIG.sock
    @flag = CONFIG.flag.to_i

    @conn = Mysql.real_connect(@host, @user, @pass, @db, @port, @sock, @flag)

    begin
    	res = @conn.query("select * from reports")
    rescue Mysql::Error => e
    	puts "Table does not exist, creating ..."
    	begin
    		@conn.query("create table reports (id INT(40) NOT NULL UNIQUE AUTO_INCREMENT, uid VARCHAR(40), event VARCHAR (20), timestamp INT UNSIGNED, timeofday TIME, dayofweek INT UNSIGNED, longitude FLOAT, latitude FLOAT, PRIMARY KEY (id) )")
    		@conn.query("create table usercluster (id INT(40) NOT NULL UNIQUE AUTO_INCREMENT, uid VARCHAR(40), context VARCHAR(40), update_timestamp INT UNSIGNED, timeofday TIME, dayofweek INT UNSIGNED, longitude FLOAT, latitude FLOAT, PRIMARY KEY (id) )")
    		puts "Done!"
    	rescue Mysql::Error => e
    		puts "Error creating new table: "+e.to_s
    	end
    end

end

def db_write(uid, event, timestamp, timeofday, dayofweek, long, lat)
	begin
		query = "INSERT INTO reports VALUES (NULL, '#{uid}', '#{event}', #{timestamp}, '#{timeofday}', '#{dayofweek}', #{long}, #{lat})"
		#p query
		@conn.query(query)
	rescue Mysql::Error => e
		puts "Error with query: "+query
		puts "Error: "+e.to_s
	end
end

def extract_loop()
	log = ""
	begin
		gzlog = File.open(CONFIG.file, mode='r')
		log = Zlib::GzipReader.new(gzlog)
	rescue
		# It's not a .gz file. Simply open it as a normal file
		log = File.open(CONFIG.file, mode='r')
	end

	begin
		#log.extend(File::Tail)
		#log.interval = 10
		#log.backward(1000000)
		#log.tail do |line|
		concatLine = ""
		splitline = ""
		whiteline = 0
		timestamp = ""
		timeofday = ""
		dayofweek = ""
		split_array = ""
		pec = false

####
# Map
####

		log.each_line do |line|

			# Pre-parser
			# This initial code will group all lines until two empty lines are found

			concatLine += line
			whiteline += 1 if /^\n/ =~ line
			
			next if whiteline < 2
			whiteline = 0

			# We get rid of the two empty lines, in the end of the concatenated string
			concatLine.strip!

			# And we split the concatenated string back into three separated lines
			splitline = concatLine.split("\n")
			concatLine = ""

			# Now we process each group of three lines
			splitline.each do |subline|

				# Here we get the server date and time
				# This information is in lines that start with the "Processing " string.
				# Some of the lines have "to json{p} ", "to html ", "to uliza " or none of those strings, so we get rid of them
				if /^Processing EngineController/ =~ subline
					# Get rid of all the "to X" in these lines
					split_array = subline.gsub(/EngineController#[\w]+ [\w]+.*.\(+?/,"EngineController# (").split(" ")
					timestamp = DateTime.strptime(split_array[5]+" "+split_array[6],"%Y-%m-%d %H:%M:%S)").to_time.to_i
					temp_tod = DateTime.parse(split_array[5]+" "+split_array[6].chop+"UTC+09:00")
					timeofday = temp_tod.new_offset('UTC-04:00').strftime('%H:%M:%S')
					dayofweek = temp_tod.new_offset('UTC-04:00').strftime('%w')
				end

				# Typically, the second and third lines have all the juice
				# Here we chose every third line that have the 200 code and geolocation info
				if /200.*.geolocation=/ =~ subline
					split_array = subline.split("&")

					# process line
					uid = split_array[1].split("=")[1]
					event = split_array[3].split("=")[1]
					split_geo_array = split_array[7].tr("=",";").split(";")

					# check if we have geo data
					# if not, either use the GeoIP or throw the data out
					next if split_geo_array[1] == nil or split_geo_array[2] == nil
					next if split_geo_array[1].empty? or split_geo_array[2].empty?
					next if split_geo_array[1] == '0.0' or split_geo_array[2] == '0.0'

					# fix nasty client code from the iOS app
					# this is due to the use of signed integers when unsigned integers should be used
					if timestamp.to_i < 0
						timestamp = (2**31 + timestamp.to_i).to_s
					end
####
# Reduce
####
					# write into db
					db_write(uid,event,timestamp,timeofday,dayofweek,split_geo_array[1],split_geo_array[2])
				end
			end
		end
	rescue Exception => e
		puts "poop!\n"+e.to_s
	end
end

init()