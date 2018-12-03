def count_chars(s)
  letters = {}
  letters.default = 0

  s.each_char { |ch|
    letters[ch] += 1
  }

  has_2 = 0
  has_3 = 0
  letters.each { |letter, cnt|
    if cnt == 2
      has_2 = 1
    elsif cnt == 3
      has_3 = 1
    end
  }

  [has_2, has_3, s]
end

def sum_tupple(a, b)
  [a[0] + b[0], a[1] + b[1]]
end

def calculate_distance(a, b)
  a.chars.
    zip(b.chars).
    select { |ca, cb| ca != cb}.
    length
end

def remove_difference(a, b)
  a.chars.
    zip(b.chars).
    select { |pair| pair[0] == pair[1] }.
    map {|pair| pair[0]}.
    join
end

def solution_1()
  File.
    open("day2.input").
    map { |s| s.strip }.
    map { |l| count_chars(l) }.
    reduce { |a, b| sum_tupple(a, b) }.
    reduce(:*).
    to_s
end

def d(s)
  puts s
  s
end

def solution_2()
  ids = File.
    open("day2.input").
    map { |s| s.strip }

  result = ids.
    product(ids).
    map {|pair| [pair[0], pair[1], calculate_distance(pair[0], pair[1])]}.
    select {|data| data[2] == 1}.
    map {|data| remove_difference(data[0], data[1])}

  result[0]
end

puts "Day 2/1: " + solution_1
puts "Day 2/2: " + solution_2
