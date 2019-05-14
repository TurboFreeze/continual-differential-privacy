### preprocess.py
### format Yelp JSON data as CSV

import json
import csv
import datetime

DATA_DIR = "data/"

DATA_NAMES = {
	"business": "business.json",
	"review": "review.json",
	"user": "user.json"
}

DATA_CSV = "yelp_data.csv"
DATA_B_CSV = "yelp_bus.csv"

REVIEW_FIELDS = ["review_id", "user_id", "business_id", "date"]
BUS_FIELDS = ["business_id", "name"]

DATE_FORMAT = "%Y-%m-%d %H:%M:%S"
DATE_START = datetime.datetime.strptime("2017-01-01 00:00:00", DATE_FORMAT)
DATE_END = datetime.datetime.strptime("2018-01-01 00:00:00", DATE_FORMAT)

max_date = datetime.datetime.min
min_date = datetime.datetime.max

# # open data file
# with open(DATA_DIR + DATA_NAMES["review"], mode='r') as input_file:
# 	# open csv destination file
# 	with open(DATA_DIR + DATA_CSV, mode='w') as output_file:
# 		writer = csv.DictWriter(output_file, fieldnames=REVIEW_FIELDS)
# 		writer.writeheader()

# 		# loop through each line
# 		for n, l in enumerate(input_file):
# 			if n % 1000000 == 0:
# 				print("Read " + str(n) + " lines")

# 			l_json = json.loads(l)

# 			# only extract a certain time period
# 			l_date = datetime.datetime.strptime(l_json["date"], DATE_FORMAT)
# 			if l_date > max_date:
# 				max_date = l_date
# 			if l_date < min_date:
# 				min_date = l_date
# 			if l_date > DATE_END or l_date < DATE_START:
# 				continue

# 			l_save = {}
# 			# extract relevant fields
# 			for field in REVIEW_FIELDS:
# 				l_save[field] = l_json[field]
# 			writer.writerow(l_save)

# print("MAX DATE: " + str(max_date))
# print("MIN DATE: " + str(min_date))


# process businesses
with open(DATA_DIR + DATA_NAMES["business"], mode='r') as input_file:
	# open csv destination file
	with open(DATA_DIR + DATA_B_CSV, mode='w') as output_file:
		writer = csv.DictWriter(output_file, fieldnames=BUS_FIELDS)
		writer.writeheader()

		# loop through each line
		for n, l in enumerate(input_file):
			if n % 1000000 == 0:
				print("Read " + str(n) + " lines")

			l_json = json.loads(l)

			l_save = {}
			# extract relevant fields
			for field in BUS_FIELDS:
				l_save[field] = l_json[field]
			writer.writerow(l_save)

		