require 'set'


class Star
  attr_accessor :x, :y, :vx, :vy

  def initialize(x, y, vx, vy)
    @x = x
    @y = y
    @vx = vx
    @vy = vy
  end

  def step()
    @x += @vx
    @y += @vy
  end

  def back()
    @x -= @vx
    @y -= @vy
  end
end


def parse_line_to_star(l)
  result = /^position=< *(.*), *(.*)> velocity=< *(.*), *(.*)>$/.match(l)

  return Star.new(result[1].to_i, result[2].to_i, result[3].to_i, result[4].to_i)
end


def get_bound(stars)
  # let
  minx = stars.map { |s| s.x }.min
  maxx = stars.map { |s| s.x }.max
  miny = stars.map { |s| s.y }.min
  maxy = stars.map { |s| s.y }.max

  # in
  [maxx - minx, maxy - miny, minx, maxx, miny, maxy]
end


def print_stars(stars)
  coords = stars.map { |s| [s.x, s.y] }.to_set
  b = get_bound(stars)

  (b[4]..b[5]).each do |y|
    (b[2]..b[3]).each do |x|
      if coords.member?([x, y])
        print '#'
      else
        print '.'
      end
    end
    puts ""
  end
end


def solution_1(filename)
  stars = File.
    open(filename).
    map { |l| l.strip() }.
    map { |l| parse_line_to_star(l) }.
    to_set

  b = get_bound(stars)
  lastw = b[0]

  (1..1000000).each do |i|
    stars.each { |s| s.step() }
    b = get_bound(stars)

    puts "#{i} - #{b[0]} x #{b[1]}"

    if b[0] > lastw
      stars.each { |s| s.back() }
      print_stars(stars)
      return
    end

    lastw = b[0]
  end
end

solution_1('day10.input')
