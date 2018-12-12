def parse_data(filename)
  f = File.open(filename)

  pots =
    f.
      readline()[15..-1].
      strip().
      chars.
      zip(0..1000).
      select { |d| d[0] == "#" }.
      map { |d| d.reverse }.
      to_h

  pots.default = "."

  f.readline()

  rules =
    f.
      map { |l| /^(.....) => (.)/.match(l) }.
      map { |r| [r[1], r[2]] }.
      to_h

  rules.default = "."

  return [pots, rules]
end


def get_part(pots, i)
  return pots[i-2] + pots[i-1] + pots[i] + pots[i+1] + pots[i+2]
end


def step_generation(pots, rules)
  nextgen = {}
  nextgen.default = "."
  minpot = pots.keys.min
  maxpot = pots.keys.max

  pots.each do |j, _|
    (j-2..j+2).each do |i|
      part = get_part(pots, i)
      if rules[part] == "#"
        nextgen[i] = "#"
      end
    end
  end

  return nextgen
end


def calculate_score(pots)
  pots.keys.reduce(:+)
end


def solution(filename, gens)
  pots, rules = parse_data(filename)

  (1..gens).each do |i|
    pots = step_generation(pots, rules)
  end

  return calculate_score(pots)
end


def solution_1(filename, gens)
  return solution(filename, gens).to_s
end


def solution_2(filename, gens)
  # Okay, apparently there is a pattern in the score after a few hundred, let's leverage that
  # let
  b1 = solution(filename, 999)
  b2 = solution(filename, 1000)
  score = b2 + (gens - 1000) * (b2 - b1)

  # in
  score.to_s
end

# solution_1('day12.example')
puts "Day 12/1: " + solution_1('day12.input', 1000)
puts "Day 12/2: " + solution_2('day12.input', 50000000000)
# puts "Day 12/2: " + solution_2('day12.input')
