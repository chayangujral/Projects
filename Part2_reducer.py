#!/usr/bin/python

import sys

hour_count = [0]*24
oldKey = None

for line in sys.stdin:
        data_mapped = line.strip().split("\t")
        if len(data_mapped) != 2:
                # Something has gone wrong. Skip this line.
                continue

        thisKey, thisHour = data_mapped
        if oldKey and oldKey != thisKey:
                for hour,count in enumerate(hour_count):
                        if count == max(hour_count):
                                print oldKey, "\t", hour
                hour_count=[0]*24

        oldKey = thisKey
        hour_count[int(thisHour)]+=1

if oldKey != None:
        for hour,count in enumerate(hour_count):
                if count == max(hour_count):
                        print oldKey, "\t", hour
