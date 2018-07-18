# Must be in same folder as rawdata.csv file. Can be run be opening terminal and typing...

# Command 1) cd 'directory', currently '/Users/timothyballard/Dropbox/Fun/Multiple-Goal Pursuit/B/Spatial/Data'
# Commend 2) python parser.py (or whatever the file is called)

# Note: will parse all csv's in file so make sure the raw data file is the only one.


__author__ = 'dpala1'

import os
import re

def main():
    # open text document
    parsed = open("parsed_data.txt", "w")
    # write first line containing variable names
    parsed.write("goal_type,experimental_goal_height,experimental_start_height,experimental_weeks_total,"
                 "experimental_crop_type,experimental_crop_side,fixed_goal_height,fixed_start_height,fixed_weeks_total,"
                 "fixed_crop_type,trial_kind,trial_number,other_key_pressed,mouse_click,trial_type,trial_index,"
                 "trial_index_global,time_elapsed,internal_chunk_id,subject_id,stage,left_current_height,"
                 "left_weeks_remaining,left_crop_type,left_goal_type,right_current_height,right_weeks_remaining,"
                 "right_crop_type,right_goal_type,mean_left_growth_given_left,sd_left_growth_given_left,"
                 "mean_left_growth_given_middle,sd_left_growth_given_middle,mean_left_growth_given_right,"
                 "sd_left_growth_given_right,mean_right_growth_given_left,sd_right_growth_given_left,"
                 "mean_right_growth_given_middle,sd_right_growth_given_middle,mean_right_growth_given_right,"
                 "sd_right_growth_given_right,response,response_time,left_growth,right_growth \n")


    age_file = open("age.txt", "w")
    age_file.write("age \n")

    gender_file = open("gender.txt", "w")
    gender_file.write("gender \n")

    # record directory of python file
    data_location = str(os.path.dirname(os.path.abspath(__file__)) + "/")

    # for all files in directory....
    for file in os.listdir(data_location):

        # if file is csv....
        if file[-4:] == ".csv":
            # open and read data
            dataf = open(data_location + file, "r")

            data_file = dataf.readlines()

            # print(data_file)
            # remove first line from datafile (which contains the names spewed out by google app engine)
            data_file.pop(0)

            # print(data_file)

            for line in data_file:
                # creates list of content, date, and key
                line_data = line.strip("\n").split("\t")

                # takes first item in list (content)
                line_relevant = line_data[0]
                # get rid of double quotes, single quotes, and curly brackets
                line_relevant = line_relevant.replace('""', '').replace('{', '').replace('"', '').replace('}', '')

                if "trial_index_global:0," in line_relevant:

                    gender = line_relevant[:line_relevant.index(",trial_type:")]
                    gender = gender[-2:]

                    if gender == "77":
                        gender_file.write('male' + "\n")

                    elif gender == "87":
                        gender_file.write('female' + "\n")

                    elif gender == "79":
                        gender_file.write('other' + "\n")

                if "trial_index_global:1," in line_relevant:

                    age = line_relevant[:line_relevant.index(",trial_type:")]
                    age = age[-3:-1]

                    age_file.write(age + "\n")

                # if given line contains 'plant-game' (i.e., if it is a trial in the experiment as opposed to an instruction).
                if "plant-game" in line_relevant:

                    # create list to store data
                    data = []

                    # extracts trial level variables (currently based on variables before stage)
                    trial_level = line_relevant[:line_relevant.index(",stage:")]
                    trial_level += line_relevant[line_relevant.index(",other_key_pressed:"):]

                    # turns it into list delimited by comma (split by variables)
                    trial_level = trial_level.split(",")

                    # create list to store trial level data
                    trial = []

                    # goes through trial level variables
                    for i in trial_level:
                        # remove variable name and semicolon from list leaving only data
                        trial.append(i[i.index(":")+1:])

                    # rejoin variables into full line
                    trial_line = (",".join(trial))

                    # handle user input data (currently between stage and trial type)
                    user_input = line_relevant[line_relevant.index("stage:"):line_relevant.index(",other_key_pressed:")]

                    # split user input variables into list
                    user_input = user_input.split("],")

                    # create store for user input data
                    user_data = []

                    # for each user input variable
                    for input in user_input:
                        # remove square brackets (replace with nothing)
                        input = input.replace('[', '').replace(']', '')

                        # remove variable name and semicolon
                        input = input[input.index(":")+1:]

                        # adding input into user_data store
                        user_data.append(input.split(","))

                    # get length of user_input (number of decision in a trial)
                    user_input_len = len(user_data[0])

                    i = 0

                    # data store for the full data
                    data = []

                    while i < user_input_len:

                        # create store for line of data
                        line = []

                        # add trial level data to store
                        line.append(trial_line)

                        # go through each variable
                        for j in user_data:

                            # extract i-th data point from j-th variable
                            line.append(j[i])

                        # add line to full data store
                        data.append(line)

                        # tick over user input counter
                        i += 1

                    # go through each line of data and write to text file (comma delimited)
                    for i in data:
                        parsed.write(",".join(i) + "\n")

            dataf.close()

    parsed.close()

main()


