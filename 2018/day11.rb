def calculate_power(x, y, serial)
  # let
  rackID = x + 10
  peek = (rackID * y + serial) * rackID
  digit = peek / 100 % 10

  # in
  digit - 5
end


def calculate_nxn_power(grid, x, y, n)
  power = 0

  (0..n-1).each do |ix|
    (0..n-1).each do |iy|
      power += grid[[x+ix, y+iy]]
    end
  end

  return power
end


def find_biggest_3x3_square(grid)
  biggest_power = -10
  biggest_coord = nil

  (1..298).each do |x|
    (1..298).each do |y|
      power = calculate_nxn_power(grid, x, y, 3)
      if biggest_power < power
        biggest_power = power
        biggest_coord = [x, y]
      end
    end
  end

  return biggest_coord
end


def build_grid(serial)
  grid = {}

  (1..300).each do |x|
    (1..300).each do |y|
      grid[[x, y]] = calculate_power(x, y, serial)
    end
  end

  return grid
end


def calculate_power_and_max_slow(grid, workboard, size)
  max_power = 0
  max_coord = nil

  (1..300-size+1).each do |x|
    (1..300-size+1).each do |y|
      power = workboard[[x, y]]

      # add right side
      (0..size-1).each do |sy|
        power += grid[[x + size - 1, y + sy]]
      end

      # bottom side, minus corner again
      (0..size-2).each do |sx|
        power += grid[[x + sx, y + size - 1]]
      end

      if power > max_power
        max_power = power
        max_coord = [x, y, size]
      end
    end
  end

  return [max_coord, max_power]
end

def calculate_power_and_max_fast(grid, workboard, size)
  max_power = nil
  max_coord = nil

  (1..300-size+1).each do |x|
    (1..300-size+1).each do |y|
      if size == 1
        power = grid[[x, y]]
      else
        power =
          grid[[x, y]] +
          workboard[[x + 1, y]][size-1] +
          workboard[[x, y + 1]][size-1] -
          workboard[[x+1, y+1]][size-2] +
          grid[[x + size - 1, y + size - 1]]
      end

      if !workboard.member?([x, y])
        workboard[[x, y]] = {}
        workboard[[x, y]].default = 0
      end

      workboard[[x, y]][size] = power
      workboard[[x, y]].delete(size - 3)  # keep memory footprint

      if max_power == nil || power > max_power
        max_power = power
        max_coord = [x, y, size]
      end
    end
  end

  return [max_coord, max_power]
end



def find_biggest_square(grid)
  workboard = {}

  max_power = nil
  max_coord = nil

  (1..300).each do |size|
    print "#{size} "
    coord, power = calculate_power_and_max_fast(grid, workboard, size)

    puts [power, coord].to_s

    if max_power == nil || power > max_power
      max_power = power
      max_coord = coord
    end
  end

  return max_coord
end


def solution_1(serial)
  grid = build_grid(serial)
  return find_biggest_3x3_square(grid)
end


def solution_2(serial)
  grid = build_grid(serial)
  return find_biggest_square(grid)
end

# puts "Day 11/1/a: " + solution_1(18).to_s + ' == [33, 45]'
# puts "Day 11/1/b: " + solution_1(42).to_s + ' == [21, 61]'
# puts "Day 11/1/c: " + solution_1(3463).to_s + ' == [235, 60]'
puts "Day 11/2/a: " + solution_2(3463).to_s + ' == [233, 282, 11]'
