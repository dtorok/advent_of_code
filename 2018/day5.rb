class Node
  attr_accessor :prev, :next
  attr_reader :value

  def initialize(value)
    @value = value
    @prev  = nil
    @next  = nil
  end
end


SENTINEL = Node.new('sentinel')


def build_linked_list(str)
  root = SENTINEL

  str.chars.each do |ch|
    node = Node.new(ch)
    node.next = root
    node.prev = nil
    root.prev = node

    root = node
  end

  root.prev = SENTINEL
  root.prev.next = root

  root = SENTINEL

  return root
end


def is_triggering(a, b)
  # puts [a, b, a.downcase == b.downcase && a != b].to_s
  return a.downcase == b.downcase && a != b
end


def print_ll(ll)
  p = ll.next
  while p != SENTINEL do
    putc p.value
    p = p.next
  end
end


def remove_from_ll(p)
  p.prev.next = p.next
  p.next.prev = p.prev
  return p.next
end

def eliminate_opposites!(polymer, exclude)
  p = polymer.next

  while p != SENTINEL do
    if p.value.downcase == exclude
      p = remove_from_ll(p)
      p = p.prev
    elsif is_triggering(p.value, p.next.value)
      p = remove_from_ll(p)
      p = remove_from_ll(p)
      if p.prev != SENTINEL
        p = p.prev
      end
    else
      p = p.next
    end
  end
end


def size_of(ll)
  cnt = 0
  p = ll.next

  while p != SENTINEL do
    cnt += 1
    p = p.next
  end

  return cnt
end


def solution_1(filename)
  polymer_string = File.read(filename).strip
  polymer = build_linked_list(polymer_string)
  eliminate_opposites!(polymer, nil)
  return size_of(polymer).to_s
end


def solution_2(filename)
  polymer_string = File.read(filename).strip

  polymer = build_linked_list(polymer_string)
  eliminate_opposites!(polymer, nil)
  min_length = size_of(polymer)

  ('a'..'z').each do |exch|
    polymer = build_linked_list(polymer_string)
    eliminate_opposites!(polymer, exch)
    length = size_of(polymer)

    if length < min_length
      min_length = length
    end
  end

  return min_length.to_s
end


puts "Day 5/1: " + solution_1('day5.input')
puts "Day 5/2: " + solution_2('day5.input')
