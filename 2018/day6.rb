require 'set'


def distance(a, b)
  return (a[0] - b[0]).abs + (a[1] - b[1]).abs
end


def mincoord(coords, index)
  coords.
    map { |i| i[index] }.
    min
end


def maxcoord(coords, index)
  coords.
    map { |i| i[index] }.
    max
end


def coord_stats(coords, from)
  # let
  pairs =
    coords.
      map { |coord| distance(from, coord) }.
      map.with_index { |d, i| [i, d] }.
      sort_by { |pair| pair[1] }

  total_distance =
    pairs.
    map { |pair| pair[1] }.
    reduce(:+)

  closest =
    if pairs[0][1] == pairs[1][1]
      nil
    else
      pairs[0][0]
    end

  # in
  [closest, total_distance]
end


def solution(filename, limit)
  # let
  coords = File.
    open(filename).
    map { |x| x.split(', ').reverse }.
    map { |x| [x[0].to_i, x[1].to_i] }

  minr = mincoord(coords, 0)
  minc = mincoord(coords, 1)
  maxr = maxcoord(coords, 0)
  maxc = maxcoord(coords, 1)

  rows = (minr..maxr).to_a
  cols = (minc..maxc).to_a

  infinite_areas = [].to_set
  area_sizes = {}
  area_sizes.default = 0

  area_size_under_limit = 0

  rows.product(cols).each do |curr|
    closest, total_distance = coord_stats(coords, curr)

    if total_distance < limit
      area_size_under_limit += 1
    end

    if closest != nil
      if [minr, maxr].member?(curr[0]) || [minc, maxc].member?(curr[1])
        infinite_areas.add(closest)
      end

      area_sizes[closest] += 1
    end
  end

  possible_areas = area_sizes.keys.to_set - infinite_areas

  closest_area_size =
    possible_areas.
      to_a.
      map { |a| area_sizes[a] }.
      max

  [closest_area_size, area_size_under_limit]
end


puts "Day 6/1: " + solution('day6.input', 0)[0].to_s
puts "Day 6/2: " + solution('day6.input', 10000)[1].to_s
