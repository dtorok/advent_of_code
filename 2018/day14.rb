load 'll.rb'

def get_new_recipes(recipes, elves)
  elves.
    map { |e| e.value }.
    reduce(:+).
    to_s.
    chars.
    map {|c| c.to_i }
end

def num_step(recipes, elf, len)
  (1 + elf.value) % len
end

def step(elf, n)
  (1..n).each do |_|
    elf = elf.next
  end

  return elf
end

def next_n(p, n)
  s = ""

  (1..n).each do |_|
    s += p.value.to_s
    p = p.next
  end

  return s
end

def matches_at(p, s)
  s.chars.each do |c|
    if c != p.value.to_s
      return false
    end
    p = p.next
  end

  return true
end

def solution(result_len, f_exit)
  recipes = create_ring(3)
  recipes.insert_after(7)

  elves = [recipes, recipes.next]

  last = recipes.next
  len = 2

  p = recipes
  pi = 0

  while true do
    get_new_recipes(recipes, elves).each do |r|
      last = last.insert_after(r)
      len += 1

      if len > result_len
        p = p.next
        pi += 1
      end

      if f_exit.(p, pi)
        return p, pi
      end
    end

    elves = elves.map { |e|
      step(e, num_step(recipes, e, len)) }
  end
end

def solution_1(n)
  p, _ = solution(10, lambda { |_, pi| pi >= n })
  return next_n(p, 10)
end

def solution_2(n)
  _, pi = solution(10, lambda { |p, _| matches_at(p, n) })
  return pi.to_s
end

puts "Day 14/1a: " + solution_1(9) + " == 5158916779"
puts "Day 14/1b: " + solution_1(5) + " == 0124515891"
puts "Day 14/1c: " + solution_1(18) + " == 9251071085"
puts "Day 14/1d: " + solution_1(2018) + " == 5941429882"
puts "Day 14/1e: " + solution_1(540391) + " == 1474315445"
puts "Day 14/2a: " + solution_2('51589') + " == 9"
puts "Day 14/2b: " + solution_2('01245') + " == 5"
puts "Day 14/2b: " + solution_2('1245') + " == 6"
puts "Day 14/2c: " + solution_2('92510') + " == 18"
puts "Day 14/2d: " + solution_2('59414') + " == 2018"
puts "Day 14/2e: " + solution_2('540391') + " == ?"
