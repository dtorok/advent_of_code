SENTINEL = {}

class Node
  attr_accessor :value, :next, :prev

  def initialize(value)
    @value = value
  end

  def insert_after(value)
    item = Node.new(value)
    item.next = @next
    item.prev = self
    @next.prev = item
    @next = item

    return item
  end

  def insert_before(value)
    item = Node.new(value)
    item.next = self
    item.prev = @prev
    @prev.next = item
    @prev = item

    return item
  end

  def is_sentinel?()
    @value == SENTINEL
  end
end

def create_ring(value)
  item = Node.new(value)
  item.next = item
  item.prev = item

  return item
end

def create_ll(value)
  Node.
    new(SENTINEL).
    create_before(value).
    create_before(SENTINEL).
    next
end
