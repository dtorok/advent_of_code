require 'set'


def parse_line(l)
  result = /^\[(\d+-\d+-\d+) \d+:(\d+)\] (.*)$/.match(l)
  date = result[1]
  minute = result[2]
  note = result[3]

  parts = note.split(' ')
  return [date, minute.to_i, parts[0], parts[1][1..-1]]
end

def register_sleeptime!(sleep_times, gid, date, from, to)
  if !sleep_times.member?(gid)
    sleep_times[gid] = {}
    sleep_times[gid].default = 0
  end

  (from..to-1).each do |min|
    sleep_times[gid][min] += 1
  end
end

def calculate_guard_sleep_times(notes)
  sleep_times = {}
  sleep_times.default = 0

  current_guard = nil
  fell_asleep = nil

  notes.each do |note|
    if note[2] == 'Guard'
      current_guard = note[3]
    elsif note[2] == 'falls'
      fell_asleep = note[1]
    elsif note[2] == 'wakes'
      date = note[0]
      min = note[1]

      register_sleeptime!(sleep_times, current_guard, date, fell_asleep, min)
    end
  end

  return sleep_times
end

def calculate_total_sleeptime(minutes)
  minutes.
    values.
    reduce(:+)
end

def choose_biggest_sleeper(sleep_times)
  max_gid = nil
  max_st = 0

  sleep_times.each do |gid, minutes|
    st = calculate_total_sleeptime(minutes)

    if !max_gid
      max_gid = gid
      max_st = st
    elsif max_st < st
      max_gid = gid
      max_st = st
    end
  end

  return max_gid, max_st
end

def pick_key_with_biggest_value(minutes)
  minutes.
    map {|min, cnt| [min, cnt]}.
    sort_by {|d| d[1]}.
    reverse.
    map {|d| d[0]}.
    first
end

def solution_1(filename)
  notes =
    File.
      open(filename).
      sort.
      map { |l| parse_line(l) }

  sleep_times = calculate_guard_sleep_times(notes)
  guard = choose_biggest_sleeper(sleep_times)
  best_min = pick_key_with_biggest_value(sleep_times[guard[0]])

  return guard[0].to_i * best_min
end

# sleep_times = {
#   "10" => {
#     1 => 1
#     2 => 1
#     3 => 4
#     4 => 1
#     ...
#   }
#   ...
# }

def solution_2(filename)
  notes =
    File.
      open(filename).
      sort.
      map { |l| parse_line(l) }

  sleep_times = calculate_guard_sleep_times(notes)

  max_slept_minutes = {}
  sleep_times.each do |gid, mins|
    min = pick_key_with_biggest_value(mins)
    max_slept_minutes[gid] = mins[min]
  end

  guard = pick_key_with_biggest_value(max_slept_minutes)
  min = pick_key_with_biggest_value(sleep_times[guard])

  return guard.to_i * min
end

puts "Day 2/1: " + solution_1('day4.input').to_s
puts "Day 2/2: " + solution_2('day4.input').to_s
