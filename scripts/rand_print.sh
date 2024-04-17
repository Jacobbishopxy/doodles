#!/usr/bin/env bash
# author:	Jacob Xie
# date:	2024/04/16 17:10:41 Tuesday
# brief: rand sleep print current time
# bash rand_print.sh -h 5 -l 1 -m 10

while getopts h:l:m: flag
do
    case "${flag}" in
        h) hight=${OPTARG};;
        l) low=${OPTARG};;
        m) timeout=${OPTARG};;
    esac
done


# Function to generate a random number between two given numbers
random_number() {
    echo $(($RANDOM % ($hight - $low + 1) + $low))
}

# Get the start time
start_time=$(date +%s)

# Start the loop
while true; do
    # Get the current time
    current_time=$(date +%H:%M:%S)

    # Print the current time
    echo "Current time: $current_time"

    # Generate a random sleep time between min_sleep and max_sleep
    sleep_time=$(random_number)

    # Sleep for the random amount of time
    sleep $sleep_time

    # Get the current time again after sleeping
    new_time=$(date +%s)

    # Calculate the elapsed time
    elapsed_time=$((new_time - start_time))

    # Check if the elapsed time exceeds the maximum timeout
    if (( $elapsed_time > $timeout )); then
        echo "Max timeout reached. Exiting..."
        break
    fi
done
