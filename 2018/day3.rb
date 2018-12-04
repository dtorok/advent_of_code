require 'set'


Claim = Struct.new(:id, :left, :top, :width, :height)

def parse_claim(line)
  data = /(.*) @ (\d+),(\d+): (\d+)x(\d+)/.match(line)
  return Claim.new(
    data[1],
    data[2].to_i, data[3].to_i,
    data[4].to_i, data[5].to_i)
end

def apply_claim(fabric, claim)
  w = claim.width
  h = claim.height

  (0..h-1).each do |r|
    (0..w-1).each do |c|
      coord_r = r + claim.top
      coord_c = c + claim.left
      key = "#{coord_r}x#{coord_c}"

      if not fabric.include?(key)
        fabric[key] = []
      end

      fabric[key] << claim.id
    end
  end

  return fabric
end

def find_standalone(fabric)
  found_alone = [].to_set
  found_together = [].to_set

  fabric.each do |key, ids|
    if ids.length == 1
      found_alone << ids[0]
    else
      found_together += ids.to_set
    end
  end

  alones = found_alone - found_together

  alones.to_a[0]
end

def solution_1(filename)
  File.
    open(filename).
    map {|l| parse_claim(l) }.
    reduce({}) { |acc, cl| apply_claim(acc, cl) }.
    select {|k,v| v.length > 1}.
    length.
    to_s
end

def solution_2(filename)
  fabric = File.
    open(filename).
    map {|l| parse_claim(l) }.
    reduce({}) { |acc, cl| apply_claim(acc, cl) }

  find_standalone(fabric).to_s
end

puts "Day 3/1: " + solution_1('day3.input')
puts "Day 3/2: " + solution_2('day3.input')
