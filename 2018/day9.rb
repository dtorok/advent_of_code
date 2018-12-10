class Marble
  attr_accessor :left, :right
  attr_reader :value

  def initialize(value)
    @value = value
    @left  = nil
    @right  = nil
  end
end


def solution_ll(num_players, limit)
  curr = Marble.new(0)
  curr.left = curr
  curr.right = curr

  scores = {}
  scores.default = 0

  curr_player = 0

  (1..limit).each do |m|
    if m % 23 != 0
      curr = curr.right

      marble = Marble.new(m)
      marble.right = curr.right
      marble.left = curr
      curr.right = marble
      marble.right.left = marble

      curr = marble
    else
      (1..7).each { |_| curr = curr.left }

      scores[curr_player] += m
      scores[curr_player] += curr.value

      curr.left.right = curr.right
      curr.right.left = curr.left

      curr = curr.right
    end
    curr_player = (curr_player + 1) % num_players
  end

  return scores.values.max.to_s
end


def solution_arr(num_players, limit)
  circle = [0]

  scores = {}
  scores.default = 0

  curr_marble = 0
  curr_player = 0

  (1..limit).each do |m|
    if m % 23 != 0
      i = (curr_marble + 2) % circle.length
      circle.insert(i, m)
      curr_marble = i
    else
      i = (curr_marble - 7) % circle.length
      scores[curr_player] += m
      scores[curr_player] += circle[i]
      circle.delete_at(i)
      curr_marble = i % circle.length
    end
    curr_player = (curr_player + 1) % num_players
  end

  return scores.values.max.to_s
end


puts solution_ll(10, 1618) + ' == 8317'
puts solution_ll(13, 7999) + ' == 146373'
puts solution_ll(17, 1104) + ' == 2764'
puts solution_ll(21, 6111) + ' == 54718'
puts solution_ll(30, 5807) + ' == 37305'
puts solution_ll(405, 70953) + ' == 422980'
puts solution_ll(405, 70953 * 100) + ' == 3552041936'
