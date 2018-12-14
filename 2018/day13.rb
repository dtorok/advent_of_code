class Collision < StandardError
  attr_accessor :carts

  def initialize(carts)
    @carts = carts
  end
end


class Board
  attr_accessor :board, :carts, :stop_at_first_crash

  def initialize(board, carts, stop_at_first_crash)
    @board = board
    @carts = carts
    @stop_at_first_crash = stop_at_first_crash
  end

  def _check_collision(cart)
    colliding = @carts.select { |c| c.c == cart.c && c.r == cart.r }
    if colliding.length > 1
      return colliding
    else
      return nil
    end
  end

  def move!()
    @carts = @carts.sort_by { |c| [c.r, c.c] }

    toremove = []

    @carts.each do |cart|
      cart.move!()
      colliding = _check_collision(cart)

      if colliding != nil
        if stop_at_first_crash
          raise Collision.new(colliding)
        else
          toremove += colliding
        end
      end
    end

    toremove.each do |c|
      @carts.delete(c)
    end

    if @carts.length == 1
      raise Collision.new(@carts)
    end
  end

  def print()
    b = @board.clone
    (0..b.length-1).each { |i| b[i] = b[i].clone }

    @carts.each do |c|
      b[c.r][c.c] = c.printdir()
    end

    b.each do |l| puts l end
  end
end


class Cart
  attr_accessor :id, :board, :r, :c, :vr, :vc

  def initialize(id, board, r, c, vr, vc)
    @id = id
    @board = board
    @r = r
    @c = c
    @vr = vr
    @vc = vc
    @rotation = 0
  end

  def _step!()
    @r += vr
    @c += vc
  end

  def _moving_horizontal?()
    @vc != 0
  end

  def _turn_left!()
    @vr, @vc = -@vc, @vr
  end

  def _turn_right!()
    @vr, @vc = @vc, -@vr
  end

  def _rotate!()
    case @rotation
    when 0
      _turn_left!()
    when 2
      _turn_right!()
    end

    @rotation = (@rotation + 1) % 3
  end

  def _turn_if_needed!()
    case @board[@r][@c]
    when "/"
      if _moving_horizontal?()
        _turn_left!()
      else
        _turn_right!()
      end
    when "\\"
      if _moving_horizontal?()
        _turn_right!()
      else
        _turn_left!()
      end
    when "+"
      _rotate!()
    end
  end

  def move!()
    _step!()
    _turn_if_needed!()
  end

  def tos()
    return "#{@id}: #{@r}, #{@c}"
  end

  def printdir()
    case [@vr, @vc]
    when [1, 0]
      return "v"
    when [-1, 0]
      return "^"
    when [0, 1]
      return ">"
    when [0, -1]
      return "<"
    end

    return "?"
  end
end


def solution(filename, stop_at_first_crash)
  board =
    File.
      open(filename).
      map { |l| l }

  carts = []
  i = 0

  (0..board.length-1).each do |r|
    (0..board[r].length).each do |c|
      ch = board[r][c]

      v = nil
      road = nil

      case ch
      when ">"
        v = [0, 1]
        road = '-'
      when "<"
        v = [0, -1]
        road = '-'
      when "v"
        v = [1, 0]
        road = '|'
      when "^"
        v = [-1, 0]
        road = '|'
      end

      if v != nil
        carts.push(Cart.new(i, board, r, c, v[0], v[1]))
        i += 1
        board[r][c] = road
      end
    end
  end

  b = Board.new(board, carts, stop_at_first_crash)

  begin
    (0..100000).each do |i|
      # b.print()
      # puts "\n\n ======== \n\n"
      b.move!()
    end
  rescue Collision => e
    c = e.carts[0]
    return "#{c.c},#{c.r}"
  end
end

puts "Day 13/1: " + solution('day13.input', true)
puts "Day 13/2: " + solution('day13.input', false)
