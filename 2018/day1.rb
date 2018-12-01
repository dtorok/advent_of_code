require 'set'


MAX_CYCLE = 9999999999


def solution_1()
  File.open("day1.input").
    map { |l| l.to_i }.
    reduce(:+).
    to_s
end


def solution_2()
  seen = [].to_set
  curr_fr = 0
  File.open("day1.input").
    map { |l| l.to_i }.
    cycle(MAX_CYCLE).each do |fr|
      curr_fr += fr
      if seen.include?(curr_fr)
        return curr_fr.to_s
      end
      seen.add(curr_fr)
    end

  "NOT FOUND"
end


puts "Solution 1: " + solution_1
puts "Solution 2: " + solution_2
