require 'set'


def parse_line(line)
  # let
  res = /^Step (\w) must be finished before step (\w) can begin/.match(line)

  # in
  [res[1], res[2]]
end


def build_conditions(data)
  steps = build_steps(data)

  conditions =
    steps.
      map { |s| [s, {}.to_set] }.
      to_h

  data.each do |step|
    a = step[0]
    b = step[1]

    conditions[b].add(a)
  end

  return conditions
end


def build_steps(data)
  data.flatten.to_set
end

def find_next_step(conditions)
  conditions.
    select { |b, as| as.length == 0 }.
    keys.
    sort.
    first
end


def remove_step!(conditions, nxt)
  conditions.delete(nxt)
  return conditions
end

def clear_step_deps!(conditions, nxt)
  conditions.each do |b, as|
    as.delete(nxt)
  end
  return conditions
end


def load_conditions(filename)
  # let =
  data =
    File.
      open(filename).
      map { |l| parse_line(l) }

  # in
  build_conditions(data)
end


def calculate_worktime(step, offset)
  offset + step.ord - 'A'.ord + 1
end


def solution_1(filename)
  conditions = load_conditions(filename)

  result = ""
  while conditions.length > 0 do
    nxt = find_next_step(conditions)
    conditions = remove_step!(conditions, nxt)
    conditions = clear_step_deps!(conditions, nxt)
    result += nxt
  end

  return result
end


def solution_2(filename, num_elves, worktime_offset)
  conditions = load_conditions(filename)

  curr_time = 0
  finish_times = []
  while conditions.length > 0 do
    nxt = find_next_step(conditions)
    if nxt != nil && num_elves > 0
      # puts [curr_time, nxt, 'start'].to_s
      worktime = calculate_worktime(nxt, worktime_offset)

      finish_times = finish_times.
        push([curr_time + worktime, nxt]).sort_by {|a| a[0]}

      conditions = remove_step!(conditions, nxt)
      num_elves -= 1
    else
      # wait for a step to finish
      done = finish_times[0]
      finish_times = finish_times[1..-1]
      curr_time = done[0]
      # puts [curr_time, done[1], 'done'].to_s
      conditions = clear_step_deps!(conditions, done[1])
      num_elves += 1
    end
  end

  curr_time = finish_times[-1][0]

  return curr_time
end


# puts "Day 7/1: " + solution_1('day7.input').to_s
puts "Day 7/2: " + solution_2('day7.input', 5, 60).to_s
